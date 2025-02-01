;;; -*- lexical-binding: t; -*-
;;; composiphrase.el --- build composable sentences, then execute them

;;; Author: William Hatch <william@hatch.uno>
;;; Maintainer: William Hatch <william@hatch.uno>
;;; Version: 0.0
;;; Homepage: https://github.com/willghatch/emacs-composiphrase
;;; Git-Repository: git://github.com/willghatch/emacs-composiphrase.git
;;; Keywords: composition modal
;;; Package-Requires: ((emacs "28"))

;;; License:
;;; This is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; <http://www.gnu.org/licenses/>

;;; Commentary:
;;;
;;; Composiphrase is a library for composing “command sentences”, and executing them.
;;; This is a similar idea to how the Vi and related text editors have a “composable editing language” with their key bindings.
;;; Except Composiphrase takes that idea more seriously.
;;; Composiphrase decouples the idea of composing elements of modifications to a command from the keys used to select words and execute sentences.
;;; Using composiphrase, in combination with a modal editing package like emacs-estate (which I wrote as a complement to composiphrase), you can create a text editing interface and language similar to Vim's.
;;; Or you can create a variety of other text editing interfaces / languages with varying degrees of composability.
;;; A complete demo editor with configuration can be seen at... TODO - I'm working on it!
;;;
;;; More concretely, composiphrase is just a simple matching library, where you write a configuration that defines verbs and objects (potentially each specifying default values for modifiers), and a big list of matchers.
;;; Matchers match on verb and object and modifiers.
;;; Matchers also specify whether and how any modifiers are passed as arguments to the matched function.
;;;
;;; By default there is no configuration -- your dreams (and, well, available time, talent, knowledge, energy, money, attention, lack of other priorities, etc) are the only limits!
;;; But there is also a demo configuration.
;;; But for now I promise no stability for the demo configuration!
;;;
;;; Details for writing a configuration:
;;; A composiphrase-sentence is a list of composiphrase-words.
;;; The `composiphrase-execute' function takes a composiphrase-sentence SENTENCE and a composiphrase-configuration CONFIG, and maps the sentence, using the config, to a specific function to call.
;;;
;;; A composiphrase-word is a dictionary imlemented as an alist, following a schema that maps a known set of keys to values.
;;; * word-type: can be 'verb, 'object, 'modifier
;;; * parameter-name: (only on 'modifier words) a symbol, name of appropriate parameter of object or verb
;;; * contents: for modifier it can be anything, for verb or object it should be a symbol
;;; * ui-hint: a string that can be displayed on the modeline or in some other way to show the current state (optional)
;;; * keys: a string or vector of the keys used when entering the word (optional)
;;;
;;; A composiphrase-configuration is an alist with the following top-level fields:
;;; * verbs - has a list of verb specifications
;;; * objects - has a list of object specifications
;;; * match-table - has a list of match-table specifications
;;;
;;; A verb specification is a list (VERB-NAME MODIFIER-SPEC ...)
;;; An object specification is a list (OBJECT-NAME MODIFIER-SPEC ...)
;;; A modifier spec is a list (PARAM-NAME DEFAULT-VALUE)
;;;
;;; A match-table specification is an list with the following fields:
;;; * verb: a symbol
;;; * object: a symbol
;;; * modifiers: an alist mapping modifier name to a list (VALUE OPTIONAL-COMPARATOR), if the optional comparator is not given, the `equal' function is used.  If `t` is given as the comparitor, the value always matches.  If a comparator function is given, it takes the given value first, then the table entry value.
;;; * executor: a list of function, then executor-argument-spec
;;;
;;; An executor-argument-spec is one of:
;;; * a list of parameter names in the order which they are to be given to the function
;;; * the symbol 'alist to pass all parameters in as an alist.
;;; * the symbol 'sentence-with-defaults to pass the the command sentence, but with default modifiers added.  This is useful to define verbs that are wrappers around others.
;;; * the symbol 'original-sentence to pass the original command sentence.
;;;
;;; Also, just go look at the demo configuration.
;;;
;;; Public API:
;;; * `composiphrase-execute'
;;; * `composiphrase-current-sentence'
;;; * `composiphrase-current-configuration'
;;; * `composiphrase-clear-current-sentence'
;;; * `composiphrase-add-to-current-sentence'
;;; * `composiphrase-add-to-current-sentence-with-numeric-handling'
;;; * `composiphrase-execute-current-sentence'
;;; * `composiphrase-current-ui-hints'

(defun composiphrase--get-spec-from-symbol (given-v-or-o given-verb-p config)
  "Takes a symbol name for a verb or an object, returns the spec from the config."
  (cdr (assq given-v-or-o
             (cdr (assq (if given-verb-p 'verbs 'objects) config)))))
(defun composiphrase--get-verb-or-obj-name (given-v-or-o)
  "Takes a composiphrase-word or a symbol."
  (if (symbolp given-v-or-o)
      given-v-or-o
    (cdr (assq 'contents given-v-or-o))))
(defun composiphrase--get-default-verb-or-obj (given-v-or-o given-verb-p config)
  "Given a verb or object (as a composiphrase-word), get the symbol for the default of the other one."
  (let* ((given-name (composiphrase--get-verb-or-obj-name given-v-or-o))
         (given-spec (composiphrase--get-spec-from-symbol given-name given-verb-p config)))
    (cdr (assq (if given-verb-p 'default-object 'default-verb) given-spec))))

(defun composiphrase-sentence-modifiers (sentence)
  "Return modifiers from SENTENCE as alist."
  (mapcar
   (lambda (word) (cons (cdr (assq 'parameter-name word))
                        (cdr (assq 'contents word))))
   (cl-remove-if-not
    (lambda (word) (eq 'modifier
                       (cdr (assq 'word-type word))))
    sentence)))

(defun composiphrase--match (sentence config)
  "Find a match for SENTENCE using the CONFIG.
Return nil if no match is found.
Otherwise, return a cons pair (PARAMS . EXECUTOR), containing the final parameters and executor from the match.
"
  (let* ((verb (seq-find (lambda (word) (eq 'verb (cdr (assq 'word-type word))))
                         sentence))
         (object (seq-find (lambda (word) (eq 'object (cdr (assq 'word-type word))))
                           sentence))
         (given-modifiers (composiphrase-sentence-modifiers sentence)))
    (when (and (not verb) (not object))
      (error "composiphrase: sentence lacks both verb and object: %s" sentence))
    ;; TODO - deeper validation of the structure of command sentence words, to be sure all parts are there.
    (let* ((verb (or verb
                     (composiphrase--get-default-verb-or-obj object nil config)
                     (error "composiphrase: can't resolve a verb for sentence: %s" sentence)))
           (object (or object
                       (composiphrase--get-default-verb-or-obj verb t config)
                       (error "composiphrase: can't resolve an object for sentence: %s" sentence)))
           (verb-name (composiphrase--get-verb-or-obj-name verb))
           (object-name (composiphrase--get-verb-or-obj-name object))
           (verb-spec (composiphrase--get-spec-from-symbol verb-name t config))
           (object-spec (composiphrase--get-spec-from-symbol object-name nil config))
           ;; TODO - check for duplicate param names, and probably error.
           (full-default-params (append verb-spec object-spec))
           (full-param-keys (seq-uniq
                             (mapcar 'car
                                     (append full-default-params
                                             given-modifiers))))
           (params (mapcar (lambda (param-name)
                             (or (assq param-name given-modifiers)
                                 (assq param-name full-default-params)))
                           full-param-keys))
           (match-table (cdr (assq 'match-table config)))
           (matched nil))
      (while (and (not matched)
                  match-table)
        (let* ((entry (car match-table))
               (verb-sym-match (eq verb-name (car entry)))
               (object-sym-match (eq object-name (cadr entry)))
               (verb-match (or verb-sym-match
                               (and (not (symbolp (car entry)))
                                    (functionp (car entry))
                                    ;; Don't try to match predicate if object
                                    ;; match already failed symbol eq
                                    (or object-sym-match (functionp (cadr entry)))
                                    (funcall (car entry) verb-name))))
               (object-match (or object-sym-match
                                 (and verb-match
                                      (not (symbolp (cadr entry)))
                                      (functionp (cadr entry))
                                      (funcall (cadr entry) object-name))))
               (vo-match (and verb-match object-match))
               (entry-mods (and vo-match
                                (caddr entry)))
               (full-match (and vo-match
                                (composiphrase--match-table-modifiers-match-p
                                 params
                                 entry-mods))))
          (when full-match
            (setq matched entry))
          (setq match-table (cdr match-table))))
      (and matched
           (cons params (seq-elt matched 3))))))


(defun composiphrase--match-table-modifiers-match-p
    (given-params-alist match-table-modifiers)
  "Check if the GIVEN-PARAMS-ALIST matches the MATCH-TABLE-MODIFIERS."
  (let ((match-failed nil))
    (while (and match-table-modifiers
                (not match-failed))
      (let* ((matcher (car match-table-modifiers))
             (param-name (car matcher))
             (given-param-value (cdr (assq param-name given-params-alist))))
        (when matcher
          (let* ((expected-value (cadr matcher))
                 (optional-comparator (caddr matcher))
                 (comparator (cond ((eq optional-comparator t)
                                    (lambda (a b) t))
                                   (optional-comparator optional-comparator)
                                   (t #'equal))))
            (when (not (funcall comparator
                                given-param-value
                                expected-value))
              (setq match-failed t)))))
      (setq match-table-modifiers (cdr match-table-modifiers)))
    (not match-failed)))

(setq composiphrase--debug-print-sentence nil)

(defun composiphrase--execute-match (orig-sentence params executor)
  "PARAMS and EXECUTOR should match what is returned from composiphrase--match."
  (let ((spec (cadr executor))
        (func (car executor)))
    (when composiphrase--debug-print-sentence
      (message "executing sentence: %s\n\nargs: %s" orig-sentence spec))
    (cond ((eq spec 'alist) (funcall func params))
          ((eq spec 'original-sentence) (funcall func orig-sentence))
          ((eq spec 'sentence-with-defaults)
           (let ((new-sentence (composiphrase--apply-params-to-sentence
                                orig-sentence params)))
             (funcall func new-sentence)))
          ((listp spec) (apply func (mapcar (lambda (name)
                                              (cdr (assq name params)))
                                            spec)))
          (t (error "bad executor spec: %s" spec)))))

(defun composiphrase-execute (sentence config)
  (let ((match (composiphrase--match sentence config)))
    (if match
        (composiphrase--execute-match sentence (car match) (cdr match))
      (error "No executor found for command sentence: %s" sentence))))

(defun composiphrase--apply-params-to-sentence (old-sentence params)
  "For each param in PARAMS that is not in OLD-SENTENCE, add the param to a new sentence (whose tail is the old sentence)."
  (let ((new-sentence old-sentence))
    (dolist (param params)
      (let ((param-name (car param))
            (param-value (cdr param)))
        (unless (seq-find (lambda (word) (and (eq (cdr (assq 'word-type word))
                                                  'modifier)
                                              (eq (cdr (assq 'parameter-name word))
                                                  param-name)))
                          old-sentence)
          (push `((word-type . modifier)
                  (parameter-name . ,param-name)
                  (contents . ,param-value))
                new-sentence))))
    new-sentence))

(defvar-local composiphrase-current-sentence nil)
(defvar composiphrase-current-configuration nil)

;; TODO - should I have just one previous sentence, or have a history that keeps up to N elements?  For now I'll do the simplest thing for my immediate wants.
(defvar-local composiphrase--previous-sentence nil)

(defun composiphrase-clear-current-sentence ()
  (interactive)
  (setq composiphrase-current-sentence nil))

(defun composiphrase-add-to-current-sentence (&rest words)
  "add words to composisphrase-current-sentence (and also add the keys used to add the command (WIP))"
  (let ((word1 (car words))
        (words-rest (cdr words)))
    (unless nil ;;no-keys
      ;; TODO - how can I get the keys for numeric arguments and uses of M-x, etc?
      (setq word1 (cons (cons 'keys (this-command-keys))
                        word1)))
    (setq composiphrase-current-sentence (append (list word1) words-rest composiphrase-current-sentence))))

(defun composiphrase-add-to-current-sentence-with-numeric-handling (exec-after-p &rest words)
  "Takes a list of composiphrase words, but returns an interactive function that takes a numeric argument, and adds the numeric argument to the modifier parameter 'num'.
If EXEC-AFTER-P is non-null, run `composiphrase-execute-current-sentence' after adding the words."
  (lambda (&optional num)
    (interactive "p")
    (apply 'composiphrase-add-to-current-sentence
           (if (and num (not (equal num 1)))
               (cons `((word-type . modifier)
                       (parameter-name . num)
                       (contents . ,num)
                       (ui-hint . ,num))
                     words)
             words))
    (when exec-after-p
      (composiphrase-execute-current-sentence))))

(defun composiphrase-execute-current-sentence ()
  "Executes composiphrase-current-sentence and clears it."
  (interactive)
  (let ((sentence composiphrase-current-sentence))
    (setq composiphrase--previous-sentence sentence)
    (composiphrase-clear-current-sentence)
    (composiphrase-execute sentence composiphrase-current-configuration)))

(defun composiphrase--keyboard-macro-from-sentence (sentence)
  "Get a vector or string of keys used to create SENTENCE."
  ;; TODO -- I need better handling to always get keys used.  I'm currently always missing keys used for numeric arguments, and I'm missing some keys used for prefix maps.  I would like at least my config to consistently work for this, even if I can't consistently get all keys in a general way that anyone could use with arbitrary configurations.
  ;; I should probably just delete this, since I decided to go a different direction for recording commands.
  (apply (lambda (&rest args)
           (apply #'seq-concatenate 'vector args))
         (mapcar (lambda (x) (if (vectorp x) x (seq--into-vector x)))
                 (seq-filter #'identity
                             (mapcar (lambda (word) (cdr (assq 'keys word)))
                                     (reverse sentence))))))

;; TODO - add composiphrase-configuration-compose that can merge configs


(defun composiphrase-current-ui-hints ()
  "Get a list of ui hints for the current command sentence."
  (let ((ui-hints (seq-filter
                   #'identity
                   (mapcar (lambda (x) (cdr (assq 'ui-hint x)))
                           composiphrase-current-sentence))))
    (reverse ui-hints)))

;; TODO - add convenient commands to add to a config, especially the default config
(defun command-sequence--config-add (config section-key spec)
  "Update the given config, mutating it."
  (let ((section (assq section-key config)))
    (setcdr section (cons spec (cdr section)))))




(provide 'composiphrase)
