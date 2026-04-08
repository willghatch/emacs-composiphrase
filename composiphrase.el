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
;;; * sentence-pre-transformers - (optional) a list of pre-transformer specifications
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
;;; Aggregation:
;;; When adding words to a sentence, you can optionally include an `accumulator'
;;; field on a modifier word.  The accumulator is a function of two arguments:
;;; (EXISTING-VALUE NEW-VALUE) -> COMBINED-VALUE.  When a word with an
;;; accumulator is added, if there is already a modifier in the sentence with
;;; the same `parameter-name', the two values are combined using the accumulator
;;; function, and the existing word is replaced rather than having two words with
;;; the same parameter-name.  If there is no existing word with that
;;; parameter-name, the accumulator is called with nil as the first argument.
;;; This aggregation happens at add-time, not at match-time.
;;;
;;; Sentence Pre-Transformers:
;;;
;;; Pre-transformers allow you to rewrite a sentence before the main match table
;;; is consulted.  This is useful for handling modifier-like concepts (such as
;;; "alternate") that change which object or verb should actually be used,
;;; without cluttering the main match table with combinatorial entries.
;;;
;;; The config key `sentence-pre-transformers' holds a list of pre-transformers.
;;; Each pre-transformer is a match table (a list of match entries) with the
;;; same structure as the main match table, except the fourth element of each
;;; entry is a transformer function instead of an executor.
;;;
;;; A pre-transformer match entry: (VERB OBJECT MODIFIERS TRANSFORMER)
;;; * VERB, OBJECT, MODIFIERS: same matching rules as the main match table.
;;; * TRANSFORMER: a function that takes two arguments -- the current sentence
;;;   and the matched params alist -- and returns a new (transformed) sentence.
;;;   The matched params alist uses holds the modifiers from the match,
;;;   including default modifiers where the modifier was absent from the
;;;   sentence.  Explicit sentence modifiers override defaults from the matched
;;;   verb and object specs.  For example, a move/word sentence could pass
;;;   ((direction . forward) (location-within . beginning)).
;;;
;;; Pre-transformers are run in order.  Each pre-transformer's match table is
;;; consulted against the current sentence; if a match is found, the transformer
;;; function is called and its return value replaces the sentence.  The same
;;; pre-transformer is then consulted again, and this repeats until that
;;; pre-transformer no longer matches.  The number of consecutive matches for
;;; each pre-transformer is bounded by
;;; `composiphrase-pre-transformer-max-matches' to avoid infinite loops.
;;; The resulting sentence is then passed to the next pre-transformer, and
;;; finally to the main matcher.
;;;
;;; The transformer function can also call (require ...) to load libraries
;;; needed by the replacement object or verb.
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
;;; * `composiphrase-verb-p'
;;; * `composiphrase-object-p'
;;; * `composiphrase-modifier-p'

(require 'cl-lib)

(defvar-local composiphrase-current-sentence nil)
(defvar composiphrase-current-configuration nil)

(defvar composiphrase-pre-transformer-max-matches 20
  "Maximum number of consecutive matches allowed for one pre-transformer.
`composiphrase--apply-pre-transformers' errors when a single
pre-transformer hits this limit while rewriting one sentence.")


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

(defun composiphrase--word-type-p (word word-type &optional parameter-name)
  "Return non-nil if WORD has WORD-TYPE.
When PARAMETER-NAME is non-nil, WORD must also have that parameter name."
  (and word
       (eq word-type (cdr (assq 'word-type word)))
       (or (null parameter-name)
           (eq parameter-name (cdr (assq 'parameter-name word))))))

(defun composiphrase-verb-p (word)
  "Return non-nil if WORD is a composiphrase verb word."
  (composiphrase--word-type-p word 'verb))

(defun composiphrase-object-p (word)
  "Return non-nil if WORD is a composiphrase object word."
  (composiphrase--word-type-p word 'object))

(cl-defun composiphrase-modifier-p (word &key parameter-name)
  "Return non-nil if WORD is a composiphrase modifier word.
When PARAMETER-NAME is non-nil, WORD must also have that parameter name."
  (composiphrase--word-type-p word 'modifier parameter-name))

(defun composiphrase-sentence-modifiers (sentence)
  "Return modifiers from SENTENCE as alist."
  (mapcar
   (lambda (word) (cons (cdr (assq 'parameter-name word))
                        (cdr (assq 'contents word))))
   (cl-remove-if-not
    (lambda (word) (eq 'modifier
                       (cdr (assq 'word-type word))))
    sentence)))

(defun composiphrase--match-context (sentence config &optional no-error)
  "Return match context for SENTENCE in CONFIG.
The return value is a list (VERB-NAME OBJECT-NAME PARAMS).
When NO-ERROR is non-nil, return nil instead of signaling an error if the
sentence cannot be resolved to both a verb and an object."
  (let* ((verb (seq-find (lambda (word) (eq 'verb (cdr (assq 'word-type word))))
                         sentence))
         (object (seq-find (lambda (word) (eq 'object (cdr (assq 'word-type word))))
                           sentence))
         (given-modifiers (composiphrase-sentence-modifiers sentence)))
    (cond
     ((and (not verb) (not object))
      (unless no-error
        (error "composiphrase: sentence lacks both verb and object: %s" sentence)))
     (t
      ;; TODO - deeper validation of the structure of command sentence words, to be sure all parts are there.
      (let* ((verb-word-or-name
              (or verb
                  (and object (composiphrase--get-default-verb-or-obj object nil config))
                  (unless no-error
                    (error "composiphrase: can't resolve a verb for sentence: %s" sentence))))
             (object-word-or-name
              (or object
                  (and verb (composiphrase--get-default-verb-or-obj verb t config))
                  (unless no-error
                    (error "composiphrase: can't resolve an object for sentence: %s" sentence)))))
        (when (and verb-word-or-name object-word-or-name)
          (let* ((verb-name (composiphrase--get-verb-or-obj-name verb-word-or-name))
                 (object-name (composiphrase--get-verb-or-obj-name object-word-or-name))
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
                                 full-param-keys)))
            (list verb-name object-name params))))))))

(defun composiphrase--match-table-word-match-p (matcher word-name)
  "Return non-nil if MATCHER matches WORD-NAME.
The symbol `_` is a wildcard for any resolved verb or object.  Other symbols
match by `eq`, and functions are called as predicates with WORD-NAME."
  (and word-name
       (cond
        ((eq matcher '_) t)
        ((symbolp matcher) (eq word-name matcher))
        ((functionp matcher) (funcall matcher word-name)))))

(defun composiphrase--match-table-entry-match-p (entry verb-name object-name params)
  "Return non-nil if match-table ENTRY matches VERB-NAME, OBJECT-NAME, and PARAMS."
  (and (composiphrase--match-table-word-match-p (car entry) verb-name)
       (composiphrase--match-table-word-match-p (cadr entry) object-name)
       (composiphrase--match-table-modifiers-match-p params (caddr entry))))

(defun composiphrase--match-table-find-match
    (match-table verb-name object-name params)
  "Return the first MATCH-TABLE entry matching VERB-NAME, OBJECT-NAME, and PARAMS."
  (let ((table match-table)
        (matched nil))
    (while (and (not matched)
                table)
      (let ((entry (car table)))
        (when (composiphrase--match-table-entry-match-p
               entry verb-name object-name params)
          (setq matched entry)))
      (setq table (cdr table)))
    matched))

(defun composiphrase--match (sentence config)
  "Find a match for SENTENCE using the CONFIG.
Return nil if no match is found.
Otherwise, return a cons pair (PARAMS . EXECUTOR), containing the final parameters and executor from the match.
"
  (let* ((context (composiphrase--match-context sentence config))
         (verb-name (car context))
         (object-name (cadr context))
         (params (caddr context))
         (matched (composiphrase--match-table-find-match
                   (cdr (assq 'match-table config))
                   verb-name object-name params)))
    (and matched
         (cons params (seq-elt matched 3)))))


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

(defun composiphrase--pre-transformer-match (sentence pre-transformer config)
  "Try to match SENTENCE against PRE-TRANSFORMER match table entries.
PRE-TRANSFORMER is a list of match entries using the same verb/object
definitions from CONFIG.  Returns nil if no match is found, or a cons
pair (PARAMS . TRANSFORMER-FUNC) if a match is found."
  ;; Pre-transformers may legitimately not match sentences that lack enough
  ;; parts to resolve both verb and object, so return nil instead of erroring.
  (let ((context (composiphrase--match-context sentence config t)))
    (when context
      (let* ((verb-name (car context))
             (object-name (cadr context))
             (params (caddr context))
             (matched (composiphrase--match-table-find-match
                       pre-transformer verb-name object-name params)))
        (and matched
             (cons params (seq-elt matched 3)))))))

(defun composiphrase--apply-pre-transformers (sentence config)
  "Apply sentence-pre-transformers from CONFIG to SENTENCE.
Pre-transformers are run in order.  Each one may repeatedly rewrite
the sentence until it no longer matches, before the next
pre-transformer (and eventually the main matcher) sees it.
Returns the (possibly transformed) sentence."
  (let ((pre-transformers (cdr (assq 'sentence-pre-transformers config)))
        (current-sentence sentence))
    (dolist (pt pre-transformers)
      (let ((pt-match (composiphrase--pre-transformer-match
                       current-sentence pt config))
            (match-count 0))
        (while pt-match
          (when (>= match-count composiphrase-pre-transformer-max-matches)
            (error "composiphrase: sentence pre-transformer matched %s times; possible infinite loop"
                   composiphrase-pre-transformer-max-matches))
          (let ((params (car pt-match))
                (transformer (cdr pt-match)))
            (setq match-count (1+ match-count))
            (setq current-sentence
                  (funcall transformer current-sentence params))
            (setq pt-match (composiphrase--pre-transformer-match
                            current-sentence pt config))))))
    current-sentence))

(defun composiphrase-execute (sentence config)
  (let* ((transformed-sentence (composiphrase--apply-pre-transformers
                                sentence config))
         (match (composiphrase--match transformed-sentence config)))
    (if match
        (composiphrase--execute-match transformed-sentence (car match) (cdr match))
      (error "No executor found for command sentence: %s" transformed-sentence))))

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



;; TODO - should I have just one previous sentence, or have a history that keeps up to N elements?  For now I'll do the simplest thing for my immediate wants.
(defvar-local composiphrase--previous-sentence nil)

(defun composiphrase-clear-current-sentence ()
  (interactive)
  (setq composiphrase-current-sentence nil))

(defun composiphrase--aggregate-word-into-sentence (word sentence)
  "Add WORD to SENTENCE, applying accumulator-based aggregation if applicable.
If WORD has an `accumulator' field and is a modifier, look for an existing
modifier in SENTENCE with the same `parameter-name'.  If found, combine the
values using the accumulator function and replace the existing word.  If not
found, call the accumulator with nil as the existing value and add the word.
Returns the new sentence."
  (let ((accumulator (cdr (assq 'accumulator word)))
        (word-type (cdr (assq 'word-type word)))
        (param-name (cdr (assq 'parameter-name word))))
    (if (and accumulator (eq word-type 'modifier) param-name)
        (let* ((existing (seq-find
                          (lambda (w)
                            (and (eq 'modifier (cdr (assq 'word-type w)))
                                 (eq param-name (cdr (assq 'parameter-name w)))))
                          sentence))
               (existing-value (and existing (cdr (assq 'contents existing))))
               (new-value (cdr (assq 'contents word)))
               (combined (funcall accumulator existing-value new-value))
               (new-word (mapcar (lambda (pair)
                                   (if (eq (car pair) 'contents)
                                       (cons 'contents combined)
                                     pair))
                                 word))
               (filtered-sentence (if existing
                                      (cl-remove-if
                                       (lambda (w)
                                         (and (eq 'modifier (cdr (assq 'word-type w)))
                                              (eq param-name (cdr (assq 'parameter-name w)))))
                                       sentence)
                                    sentence)))
          (cons new-word filtered-sentence))
      (cons word sentence))))

(defun composiphrase-add-to-current-sentence (&rest words)
  "add words to composisphrase-current-sentence (and also add the keys used to add the command (WIP))
When a word has an `accumulator' field, aggregation is applied: see the
commentary section on Aggregation for details."
  (let ((word1 (car words))
        (words-rest (cdr words)))
    (unless nil ;;no-keys
      ;; TODO - how can I get the keys for numeric arguments and uses of M-x, etc?
      (setq word1 (cons (cons 'keys (this-command-keys))
                        word1)))
    (let ((new-sentence composiphrase-current-sentence))
      (dolist (w (append (list word1) words-rest))
        (setq new-sentence (composiphrase--aggregate-word-into-sentence w new-sentence)))
      (setq composiphrase-current-sentence new-sentence))))

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
