;;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'composiphrase)


(setq composiphrase--test-config
      `((verbs
         .
         ((move (default-object . word) (direction . forward) (count . 1))
          (delete (default-object . word) (direction . forward))
          (arpeggiate (defaut-object . word))
          ))
        (objects
         .
         ((word (default-verb . move) (location-within . beginning))
          (sentence (default-verb . move) (location-within . beginning))
          ))
        (match-table
         .
         ((move word
                ((direction forward) (location-within beginning eq))
                (forward-word (count)))
          (move sentence
                ((unlisted-mod foo))
                (unlisted-sentence-move-func (count)))
          (arpeggiate sentence
                      ((unlisted-mod ,nil))
                      (unlisted-nil-sentence-arpeggiate))
          (delete ,(lambda (x) t)
                  ()
                  (function-to-delete-after-moving sentence-with-defaults)))
         )))



(ert-deftest composiphrase--match-test ()
  "Test matching functionality of `composiphrase--match`."
  (let ((mv '((word-type . verb)
              (contents . move)))
        (del '((word-type . verb)
               (contents . delete)))
        (wd '((word-type . object)
              (contents . word)))
        (fwd '((word-type . modifier)
               (parameter-name . direction)
               (contents . forward)))
        (beg '((word-type . modifier)
               (parameter-name . location-within)
               (contents . beginning))))

    (let ((result1 (composiphrase--match (list mv fwd beg wd) composiphrase--test-config)))
      (should result1)
      (let ((params (car result1))
            (executor (cdr result1)))
        (should (equal 'forward (cdr (assq 'direction params))))
        (should (equal 'beginning (cdr (assq 'location-within params))))
        (should (equal 'forward-word (car executor)))
        ))

    ;; Same as above, but with default values
    (let ((result2 (composiphrase--match (list mv wd) composiphrase--test-config)))
      (should result2)
      (let ((params (car result2))
            (executor (cdr result2)))
        (should (equal 'forward (cdr (assq 'direction params))))
        (should (equal 'beginning (cdr (assq 'location-within params))))
        (should (equal 'forward-word (car executor)))
        ))

    ;; Predicate matcher
    (let ((result3 (composiphrase--match (list del wd) composiphrase--test-config)))
      (should result3)
      (let ((params (car result3))
            (executor (cdr result3)))
        ;;(should (equal 'forward (cdr (assq 'direction params))))
        ;;(should (equal 'beginning (cdr (assq 'location-within params))))
        (should (equal 'function-to-delete-after-moving (car executor)))
        ))
    )


  (let ((non-matching-sentence '(((word-type . verb)
                                  (contents . jump))
                                 ((word-type . object)
                                  (contents . word))
                                 ((word-type . modifier)
                                  (parameter-name . direction)
                                  (contents . backward)))))
    (should-not (composiphrase--match non-matching-sentence composiphrase--test-config)))

  (let ((sentence-with-unlisted-mod-match
         '(((word-type . verb)
            (contents . move))
           ((word-type . object)
            (contents . sentence))
           ((word-type . modifier)
            (parameter-name . unlisted-mod)
            (contents . foo)))))
    (should (composiphrase--match sentence-with-unlisted-mod-match
                                  composiphrase--test-config)))

  (let ((sentence-with-unlisted-mod-no-default
         '(((word-type . verb)
            (contents . move))
           ((word-type . object)
            (contents . sentence))
           ;; Note the lack of any explicit unlisted-mod value, and also it has no default value.
           )))
    (should-not (composiphrase--match sentence-with-unlisted-mod-no-default
                                      composiphrase--test-config)))
  (let ((sentence-with-unlisted-mod-no-default-nil-match
         '(((word-type . verb)
            (contents . arpeggiate))
           ((word-type . object)
            (contents . sentence))
           ;; Note the lack of any explicit unlisted-mod value, and also it has no default value.
           ;; But this one should match because the matcher looks for nil.
           )))
    (should (composiphrase--match sentence-with-unlisted-mod-no-default-nil-match
                                  composiphrase--test-config)))

  (let ((sentence-with-unlisted-mod-doesnt-match-nil
         '(((word-type . verb)
            (contents . arpeggiate))
           ((word-type . object)
            (contents . sentence))
           ((word-type . modifier)
            (parameter-name . unlisted-mod)
            (contents . foo))
           )))
    (should-not (composiphrase--match sentence-with-unlisted-mod-doesnt-match-nil
                                      composiphrase--test-config)))
  )

(ert-deftest composiphrase--match-wildcard-verb-and-object-test ()
  "Test that `_` matches any resolved verb or object in normal matching."
  (let ((config
         '((verbs
            .
            ((move (default-object . word) (direction . forward))
             (delete (default-object . word) (direction . forward))))
           (objects
            .
            ((word (default-verb . move))
             (sentence (default-verb . move))))
           (match-table
            .
            ((_ sentence () (any-verb-sentence ()))
             (move _ ((direction forward)) (move-any-object ())))))))
    (let ((verb-wildcard-match
           (composiphrase--match
            '(((word-type . verb) (contents . delete))
              ((word-type . object) (contents . sentence)))
            config)))
      (should verb-wildcard-match)
      (should (eq 'any-verb-sentence (car (cdr verb-wildcard-match)))))
    (let ((object-wildcard-match
           (composiphrase--match
            '(((word-type . verb) (contents . move))
              ((word-type . object) (contents . word)))
            config)))
      (should object-wildcard-match)
      (should (eq 'move-any-object (car (cdr object-wildcard-match)))))))

(ert-deftest composiphrase-word-predicate-test ()
  "Test public predicates for composiphrase words."
  (let ((verb '((word-type . verb) (contents . move)))
        (object '((word-type . object) (contents . word)))
        (modifier '((word-type . modifier)
                    (parameter-name . direction)
                    (contents . forward))))
    (should (composiphrase-verb-p verb))
    (should-not (composiphrase-object-p verb))
    (should (composiphrase-object-p object))
    (should-not (composiphrase-modifier-p object))
    (should (composiphrase-modifier-p modifier))
    (should (composiphrase-modifier-p modifier :parameter-name 'direction))
    (should-not (composiphrase-modifier-p modifier :parameter-name 'count))))


;;; Aggregation tests

(defun composiphrase-test-current-sentence-from-words (&rest words)
  "Return a current sentence built by adding WORDS through the public API."
  (with-temp-buffer
    (composiphrase-clear-current-sentence)
    (dolist (word words)
      (composiphrase-add-to-current-sentence word))
    composiphrase-current-sentence))

(ert-deftest composiphrase-add-to-current-sentence-sum-accumulator-test ()
  "Test that numeric values are summed when using a + accumulator."
  (let* ((sum-accumulator (lambda (existing new)
                            (+ (or existing 0) new)))
         (word-a `((word-type . modifier)
                   (parameter-name . count)
                   (contents . 3)
                   (accumulator . ,sum-accumulator)))
         (word-b `((word-type . modifier)
                   (parameter-name . count)
                   (contents . 2)
                   (accumulator . ,sum-accumulator)))
         (result (composiphrase-test-current-sentence-from-words
                  word-a word-b)))
    ;; The sentence should have exactly one modifier with parameter-name count.
    (let ((count-words (cl-remove-if-not
                        (lambda (w)
                          (and (eq 'modifier (cdr (assq 'word-type w)))
                               (eq 'count (cdr (assq 'parameter-name w)))))
                        result)))
      (should (= 1 (length count-words)))
      ;; The accumulated value should be 3 + 2 = 5.
      (should (= 5 (cdr (assq 'contents (car count-words))))))))

(ert-deftest composiphrase-add-to-current-sentence-nil-existing-accumulator-test ()
  "Test that the accumulator is called with nil when no prior word exists."
  (let* ((sum-accumulator (lambda (existing new)
                            (+ (or existing 0) new)))
         (word `((word-type . modifier)
                 (parameter-name . count)
                 (contents . 7)
                 (accumulator . ,sum-accumulator)))
         (result (composiphrase-test-current-sentence-from-words word)))
    (should (= 1 (length result)))
    ;; accumulator called with (nil, 7), which should produce 7.
    (should (= 7 (cdr (assq 'contents (car result)))))))

(ert-deftest composiphrase-add-to-current-sentence-list-append-accumulator-test ()
  "Test aggregation with a list-append accumulator."
  (let* ((list-accumulator (lambda (existing new)
                             (append (or existing '()) (list new))))
         (word-a `((word-type . modifier)
                   (parameter-name . tags)
                   (contents . alpha)
                   (accumulator . ,list-accumulator)))
         (word-b `((word-type . modifier)
                   (parameter-name . tags)
                   (contents . beta)
                   (accumulator . ,list-accumulator)))
         (word-c `((word-type . modifier)
                   (parameter-name . tags)
                   (contents . gamma)
                   (accumulator . ,list-accumulator)))
         (result (composiphrase-test-current-sentence-from-words
                  word-a word-b word-c)))
    ;; Should have one tags modifier with all three values.
    (let ((tags-words (cl-remove-if-not
                       (lambda (w)
                         (and (eq 'modifier (cdr (assq 'word-type w)))
                              (eq 'tags (cdr (assq 'parameter-name w)))))
                       result)))
      (should (= 1 (length tags-words)))
      (should (equal '(alpha beta gamma)
                     (cdr (assq 'contents (car tags-words))))))))

(ert-deftest composiphrase-add-to-current-sentence-no-accumulator-test ()
  "Test that words without an accumulator are added normally, without aggregation."
  (let* ((word-a '((word-type . modifier)
                   (parameter-name . direction)
                   (contents . forward)))
         (word-b '((word-type . modifier)
                   (parameter-name . direction)
                   (contents . backward)))
         (result (composiphrase-test-current-sentence-from-words
                  word-a word-b)))
    ;; Without an accumulator, both words should be present.
    (let ((direction-words (cl-remove-if-not
                            (lambda (w)
                              (and (eq 'modifier (cdr (assq 'word-type w)))
                                   (eq 'direction (cdr (assq 'parameter-name w)))))
                            result)))
      (should (= 2 (length direction-words))))))

(ert-deftest composiphrase-add-to-current-sentence-non-modifier-unaffected-test ()
  "Test that verbs and objects with an accumulator field are not aggregated."
  (let* ((verb-a `((word-type . verb)
                   (contents . move)
                   (accumulator . ,(lambda (a b) b))))
         (verb-b `((word-type . verb)
                   (contents . delete)
                   (accumulator . ,(lambda (a b) b))))
         (result (composiphrase-test-current-sentence-from-words
                  verb-a verb-b)))
    ;; Both verbs should be present; accumulator is ignored for non-modifiers.
    (should (= 2 (length result)))))

(ert-deftest composiphrase-add-to-current-sentence-preserves-other-words-test ()
  "Test that aggregation only affects matching modifiers, leaving other words intact."
  (let* ((sum-accumulator (lambda (existing new)
                            (+ (or existing 0) new)))
         (verb '((word-type . verb) (contents . move)))
         (object '((word-type . object) (contents . word)))
         (other-mod '((word-type . modifier)
                      (parameter-name . direction)
                      (contents . forward)))
         (count-a `((word-type . modifier)
                    (parameter-name . count)
                    (contents . 3)
                    (accumulator . ,sum-accumulator)))
         (count-b `((word-type . modifier)
                    (parameter-name . count)
                    (contents . 4)
                    (accumulator . ,sum-accumulator)))
         (result (composiphrase-test-current-sentence-from-words
                  verb object other-mod count-a count-b)))
    ;; Should have 4 words: verb, object, direction modifier, aggregated count.
    (should (= 4 (length result)))
    ;; The count should be 3 + 4 = 7.
    (let ((count-word (seq-find
                       (lambda (w)
                         (and (eq 'modifier (cdr (assq 'word-type w)))
                              (eq 'count (cdr (assq 'parameter-name w)))))
                       result)))
      (should count-word)
      (should (= 7 (cdr (assq 'contents count-word)))))
    ;; The other modifier should still be there.
    (let ((dir-word (seq-find
                     (lambda (w)
                       (and (eq 'modifier (cdr (assq 'word-type w)))
                            (eq 'direction (cdr (assq 'parameter-name w)))))
                     result)))
      (should dir-word)
      (should (eq 'forward (cdr (assq 'contents dir-word)))))))

(ert-deftest composiphrase-add-to-current-sentence-accumulator-preserved-on-result-test ()
  "Test that the accumulator field is preserved on the resulting word after aggregation."
  (let* ((sum-accumulator (lambda (existing new)
                            (+ (or existing 0) new)))
         (word-a `((word-type . modifier)
                   (parameter-name . count)
                   (contents . 2)
                   (accumulator . ,sum-accumulator)))
         (word-b `((word-type . modifier)
                   (parameter-name . count)
                   (contents . 3)
                   (accumulator . ,sum-accumulator)))
         (result (composiphrase-test-current-sentence-from-words
                  word-a word-b)))
    ;; After aggregation, the word should still have its accumulator,
    ;; allowing further aggregations.
    (let ((count-word (car result)))
      (should (assq 'accumulator count-word))
      ;; And we can aggregate again.
      (let* ((word-c `((word-type . modifier)
                       (parameter-name . count)
                       (contents . 10)
                       (accumulator . ,sum-accumulator)))
             (continued-result (composiphrase-test-current-sentence-from-words
                                count-word word-c)))
        (should (= 15 (cdr (assq 'contents (car continued-result)))))))))

(ert-deftest composiphrase-add-to-current-sentence-custom-accumulator-test ()
  "Test aggregation with a custom accumulator that takes the max of two values."
  (let* ((max-accumulator (lambda (existing new)
                            (max (or existing 0) new)))
         (word-a `((word-type . modifier)
                   (parameter-name . priority)
                   (contents . 5)
                   (accumulator . ,max-accumulator)))
         (word-b `((word-type . modifier)
                   (parameter-name . priority)
                   (contents . 3)
                   (accumulator . ,max-accumulator)))
         (word-c `((word-type . modifier)
                   (parameter-name . priority)
                   (contents . 8)
                   (accumulator . ,max-accumulator)))
         (result (composiphrase-test-current-sentence-from-words
                  word-a word-b word-c)))
    (should (= 1 (length result)))
    ;; max(max(5, 3), 8) = max(5, 8) = 8
    (should (= 8 (cdr (assq 'contents (car result)))))))

;;; ============================================================
;;; Sentence Pre-Transformer Tests
;;; ============================================================

;; A config modeling the "alternate" incrementing use case:
;; Instead of writing match entries for every combination of
;; (alternate x word), (alternate-2 x word), etc., we write
;; pre-transformers that rewrite "alternate + word" into
;; "cpo-vi-like-word", and the main match table only needs to
;; handle the concrete object types.
(setq composiphrase--pre-transformer-test-config
      `((verbs
         .
         ((move (default-object . word) (direction . forward) (count . 1))
          (delete (default-object . word) (direction . forward))
          ))
        (objects
         .
         ((word (default-verb . move) (location-within . beginning))
          (cpo-vi-like-word (default-verb . move) (location-within . beginning))
          (big-word (default-verb . move) (location-within . beginning))
          ))
        (sentence-pre-transformers
         .
         (;; Pre-transformer 1: When "alternate" modifier is present
          ;; and the object is "word", replace the object with
          ;; "cpo-vi-like-word" and remove the "alternate" modifier.
          ((_ word
              ((alternate t))
              ,(lambda (sentence _params)
                 (cons
                  '((word-type . object) (contents . cpo-vi-like-word))
                  (cl-remove-if
                   (lambda (word)
                     (or (composiphrase-object-p word)
                         (composiphrase-modifier-p
                          word :parameter-name 'alternate)))
                   sentence))))
           ;; Pre-transformer 1 also handles: alternate + big-word is an error
           ;; (no match entry for that, so it falls through)
           )
          ;; Pre-transformer 2: A second pre-transformer that shows
          ;; ordering -- if a "double" modifier is present, double the
          ;; count modifier.
          ((_ _
              ((double t))
              ,(lambda (sentence params)
                 (let ((count-val (or (cdr (assq 'count params)) 1)))
                   (cons
                    `((word-type . modifier)
                      (parameter-name . count)
                      (contents . ,(* 2 count-val)))
                    (cl-remove-if
                     (lambda (word)
                       (composiphrase-modifier-p
                        word :parameter-name 'double))
                     sentence))))))
          ))
        (match-table
         .
         ((move word
                ((direction forward) (location-within beginning eq))
                (forward-word (count)))
          (move cpo-vi-like-word
                ((direction forward) (location-within beginning eq))
                (forward-cpo-vi-like-word (count)))
          (delete word
                  ()
                  (delete-word (count)))
          (delete cpo-vi-like-word
                  ()
                  (delete-cpo-vi-like-word (count)))
          ))
        ))


(ert-deftest composiphrase--pre-transformer-basic-rewrite ()
  "Test that a pre-transformer rewrites 'alternate + word' to 'cpo-vi-like-word'."
  ;; Sentence: move alternate word
  ;; Pre-transformer should rewrite to: move cpo-vi-like-word
  ;; Main matcher should then match: move cpo-vi-like-word -> forward-cpo-vi-like-word
  (let* ((sentence (list '((word-type . verb) (contents . move))
                         '((word-type . object) (contents . word))
                         '((word-type . modifier) (parameter-name . alternate) (contents . t))))
         (transformed (composiphrase--apply-pre-transformers
                       sentence composiphrase--pre-transformer-test-config)))
    ;; The object should now be cpo-vi-like-word
    (let ((obj (seq-find (lambda (w) (and w (eq 'object (cdr (assq 'word-type w)))))
                         transformed)))
      (should obj)
      (should (eq 'cpo-vi-like-word (cdr (assq 'contents obj)))))
    ;; The alternate modifier should be gone (removed, replaced with nil)
    (let ((alt-mod (seq-find (lambda (w) (and w
                                              (eq 'modifier (cdr (assq 'word-type w)))
                                              (eq 'alternate (cdr (assq 'parameter-name w)))))
                             transformed)))
      (should-not alt-mod))))

(ert-deftest composiphrase--pre-transformer-no-match-passthrough ()
  "Test that a sentence not matching any pre-transformer passes through unchanged."
  ;; Sentence: move word (no alternate modifier)
  ;; No pre-transformer should match, so the sentence should be unchanged.
  (let* ((sentence (list '((word-type . verb) (contents . move))
                         '((word-type . object) (contents . word))))
         (transformed (composiphrase--apply-pre-transformers
                       sentence composiphrase--pre-transformer-test-config)))
    (should (equal sentence transformed))))

(ert-deftest composiphrase--pre-transformer-execute-integration ()
  "Test that composiphrase-execute correctly uses pre-transformers
to rewrite 'alternate + word' and dispatch to the right executor."
  ;; Sentence: move alternate word
  ;; After pre-transformation: move cpo-vi-like-word
  ;; Executor should be: forward-cpo-vi-like-word
  (let* ((sentence (list '((word-type . verb) (contents . move))
                         '((word-type . object) (contents . word))
                         '((word-type . modifier) (parameter-name . alternate) (contents . t))))
         (match (composiphrase--match
                 (composiphrase--apply-pre-transformers
                  sentence composiphrase--pre-transformer-test-config)
                 composiphrase--pre-transformer-test-config)))
    (should match)
    (should (eq 'forward-cpo-vi-like-word (car (cdr match))))))

(ert-deftest composiphrase--pre-transformer-ordering ()
  "Test that pre-transformers run in order.
When both 'alternate' and 'double' modifiers are present:
  1. First pre-transformer rewrites word -> cpo-vi-like-word, removes alternate
  2. Second pre-transformer doubles the count, removes double modifier"
  (let* ((sentence (list '((word-type . verb) (contents . move))
                         '((word-type . object) (contents . word))
                         '((word-type . modifier) (parameter-name . alternate) (contents . t))
                         '((word-type . modifier) (parameter-name . double) (contents . t))
                         '((word-type . modifier) (parameter-name . count) (contents . 3))))
         (transformed (composiphrase--apply-pre-transformers
                       sentence composiphrase--pre-transformer-test-config)))
    ;; Object should be rewritten to cpo-vi-like-word
    (let ((obj (seq-find (lambda (w) (and w (eq 'object (cdr (assq 'word-type w)))))
                         transformed)))
      (should obj)
      (should (eq 'cpo-vi-like-word (cdr (assq 'contents obj)))))
    ;; alternate modifier should be gone
    (let ((alt-mod (seq-find (lambda (w) (and w
                                              (eq 'modifier (cdr (assq 'word-type w)))
                                              (eq 'alternate (cdr (assq 'parameter-name w)))))
                             transformed)))
      (should-not alt-mod))
    ;; double modifier should be gone
    (let ((dbl-mod (seq-find (lambda (w) (and w
                                              (eq 'modifier (cdr (assq 'word-type w)))
                                              (eq 'double (cdr (assq 'parameter-name w)))))
                             transformed)))
      (should-not dbl-mod))
    ;; count should be doubled (3 -> 6)
    ;; The doubled count is added as a new modifier word at the front
    (let ((count-mods (seq-filter (lambda (w) (and w
                                                   (eq 'modifier (cdr (assq 'word-type w)))
                                                   (eq 'count (cdr (assq 'parameter-name w)))))
                                  transformed)))
      ;; There may be two count modifiers (the new doubled one and the original),
      ;; but the first one (added by the transformer) should have the doubled value.
      (should (>= (length count-mods) 1))
      (should (equal 6 (cdr (assq 'contents (car count-mods))))))))

(ert-deftest composiphrase--pre-transformer-repeats-until-no-match ()
  "Test that one pre-transformer can apply multiple matching entries."
  (let* ((object-transformer
          (lambda (sentence _params)
            (mapcar (lambda (word)
                      (cond
                       ((and (composiphrase-object-p word)
                             (eq 'word (cdr (assq 'contents word))))
                        '((word-type . object) (contents . cpo-vi-like-word)))
                       ((composiphrase-modifier-p
                         word :parameter-name 'alternate)
                        nil)
                       (t word)))
                    sentence)))
         (verb-transformer
          (lambda (sentence _params)
            (mapcar (lambda (word)
                      (cond
                       ((and (composiphrase-verb-p word)
                             (eq 'move (cdr (assq 'contents word))))
                        '((word-type . verb) (contents . delete)))
                       ((composiphrase-modifier-p
                         word :parameter-name 'verb-alternate)
                        nil)
                       (t word)))
                    sentence)))
         (config
          `((verbs
             .
             ((move (default-object . word))
              (delete (default-object . word))))
            (objects
             .
             ((word (default-verb . move))
              (cpo-vi-like-word (default-verb . move))))
            (sentence-pre-transformers
             .
             (((_ word
                  ((alternate t))
                  ,object-transformer)
               (_ cpo-vi-like-word
                  ((verb-alternate t))
                  ,verb-transformer))))
            (match-table
             .
             ((delete cpo-vi-like-word () (delete-cpo-vi-like-word ()))))))
         (sentence (list '((word-type . verb) (contents . move))
                         '((word-type . object) (contents . word))
                         '((word-type . modifier) (parameter-name . alternate) (contents . t))
                         '((word-type . modifier) (parameter-name . verb-alternate) (contents . t))))
         (transformed (composiphrase--apply-pre-transformers sentence config)))
    (let ((verb (seq-find (lambda (w) (and w (eq 'verb (cdr (assq 'word-type w)))))
                          transformed))
          (object (seq-find (lambda (w) (and w (eq 'object (cdr (assq 'word-type w)))))
                            transformed)))
      (should (eq 'delete (cdr (assq 'contents verb))))
      (should (eq 'cpo-vi-like-word (cdr (assq 'contents object)))))
    (should-not (seq-find (lambda (w) (and w
                                           (eq 'modifier (cdr (assq 'word-type w)))
                                           (eq 'alternate (cdr (assq 'parameter-name w)))))
                          transformed))
    (should-not (seq-find (lambda (w) (and w
                                           (eq 'modifier (cdr (assq 'word-type w)))
                                           (eq 'verb-alternate (cdr (assq 'parameter-name w)))))
                          transformed))
    (let ((match (composiphrase--match transformed config)))
      (should match)
      (should (eq 'delete-cpo-vi-like-word (car (cdr match)))))))

(ert-deftest composiphrase--pre-transformer-errors-at-repeat-limit ()
  "Test that an always-matching pre-transformer errors at the match limit."
  (let* ((identity-transformer (lambda (sentence _params) sentence))
         (config
          `((verbs . ((move (default-object . word))))
            (objects . ((word (default-verb . move))))
            (sentence-pre-transformers
             .
             (((,(lambda (_v) t) word
                ()
                ,identity-transformer))))
            (match-table . ((move word () (move-word ()))))))
         (sentence (list '((word-type . verb) (contents . move))
                         '((word-type . object) (contents . word)))))
    (let ((composiphrase-pre-transformer-max-matches 2))
      (let ((err (should-error
                  (composiphrase--apply-pre-transformers sentence config)
                  :type 'error)))
        (should
         (string-match-p
          "sentence pre-transformer matched 2 times"
          (error-message-string err)))))))

(ert-deftest composiphrase--pre-transformer-only-double ()
  "Test that the 'double' pre-transformer works alone (without 'alternate')."
  (let* ((sentence (list '((word-type . verb) (contents . move))
                         '((word-type . object) (contents . word))
                         '((word-type . modifier) (parameter-name . double) (contents . t))
                         '((word-type . modifier) (parameter-name . count) (contents . 5))))
         (transformed (composiphrase--apply-pre-transformers
                       sentence composiphrase--pre-transformer-test-config)))
    ;; Object should still be word (no alternate modifier)
    (let ((obj (seq-find (lambda (w) (and w (eq 'object (cdr (assq 'word-type w)))))
                         transformed)))
      (should obj)
      (should (eq 'word (cdr (assq 'contents obj)))))
    ;; double modifier should be gone
    (let ((dbl-mod (seq-find (lambda (w) (and w
                                              (eq 'modifier (cdr (assq 'word-type w)))
                                              (eq 'double (cdr (assq 'parameter-name w)))))
                             transformed)))
      (should-not dbl-mod))
    ;; count should be doubled (5 -> 10)
    (let ((count-mods (seq-filter (lambda (w) (and w
                                                   (eq 'modifier (cdr (assq 'word-type w)))
                                                   (eq 'count (cdr (assq 'parameter-name w)))))
                                  transformed)))
      (should (>= (length count-mods) 1))
      (should (equal 10 (cdr (assq 'contents (car count-mods))))))))

(ert-deftest composiphrase--pre-transformer-with-delete-verb ()
  "Test that pre-transformer works with a different verb (delete)."
  ;; Sentence: delete alternate word
  ;; Pre-transformer: rewrites to delete cpo-vi-like-word
  (let* ((sentence (list '((word-type . verb) (contents . delete))
                         '((word-type . object) (contents . word))
                         '((word-type . modifier) (parameter-name . alternate) (contents . t))))
         (transformed (composiphrase--apply-pre-transformers
                       sentence composiphrase--pre-transformer-test-config)))
    (let ((obj (seq-find (lambda (w) (and w (eq 'object (cdr (assq 'word-type w)))))
                         transformed)))
      (should obj)
      (should (eq 'cpo-vi-like-word (cdr (assq 'contents obj)))))
    ;; And it should match in the main table
    (let ((match (composiphrase--match transformed composiphrase--pre-transformer-test-config)))
      (should match)
      (should (eq 'delete-cpo-vi-like-word (car (cdr match)))))))

(ert-deftest composiphrase--pre-transformer-no-transformers-in-config ()
  "Test that a config without sentence-pre-transformers works fine."
  ;; The original test config has no sentence-pre-transformers key.
  ;; composiphrase--apply-pre-transformers should just return the sentence unchanged.
  (let* ((sentence (list '((word-type . verb) (contents . move))
                         '((word-type . object) (contents . word))))
         (transformed (composiphrase--apply-pre-transformers
                       sentence composiphrase--test-config)))
    (should (equal sentence transformed))))

(ert-deftest composiphrase--pre-transformer-empty-list ()
  "Test that an empty sentence-pre-transformers list works fine."
  (let* ((config `((verbs . ((move (direction . forward))))
                   (objects . ((word (default-verb . move))))
                   (sentence-pre-transformers . ())
                   (match-table . ((move word () (forward-word ()))))))
         (sentence (list '((word-type . verb) (contents . move))
                         '((word-type . object) (contents . word))))
         (transformed (composiphrase--apply-pre-transformers sentence config)))
    (should (equal sentence transformed))))

(ert-deftest composiphrase--pre-transformer-symbol-verb-match ()
  "Test pre-transformer matching with a specific verb symbol (not a predicate)."
  (let* ((config
          `((verbs . ((move (direction . forward) (count . 1))
                      (yank (direction . forward) (count . 1))))
            (objects . ((word (default-verb . move))
                        (big-word (default-verb . move))))
            (sentence-pre-transformers
             .
             (;; Only rewrite for 'move verb, not for 'yank
              ((move word
                     ((alternate t))
                     ,(lambda (sentence _params)
                        (mapcar (lambda (w)
                                  (if (and w
                                           (eq 'object (cdr (assq 'word-type w)))
                                           (eq 'word (cdr (assq 'contents w))))
                                      '((word-type . object) (contents . big-word))
                                    w))
                                sentence))))))
            (match-table
             .
             ((move word () (move-word ()))
              (move big-word () (move-big-word ()))
              (yank word () (yank-word ()))))))
         ;; move + alternate + word -> should be rewritten to move + big-word
         (sentence-move (list '((word-type . verb) (contents . move))
                              '((word-type . object) (contents . word))
                              '((word-type . modifier) (parameter-name . alternate) (contents . t))))
         ;; yank + alternate + word -> should NOT be rewritten (verb doesn't match)
         (sentence-yank (list '((word-type . verb) (contents . yank))
                              '((word-type . object) (contents . word))
                              '((word-type . modifier) (parameter-name . alternate) (contents . t)))))
    ;; move + alternate + word -> big-word
    (let ((transformed (composiphrase--apply-pre-transformers sentence-move config)))
      (let ((obj (seq-find (lambda (w) (and w (eq 'object (cdr (assq 'word-type w)))))
                           transformed)))
        (should (eq 'big-word (cdr (assq 'contents obj))))))
    ;; yank + alternate + word -> still word (no match in pre-transformer)
    (let ((transformed (composiphrase--apply-pre-transformers sentence-yank config)))
      (let ((obj (seq-find (lambda (w) (and w (eq 'object (cdr (assq 'word-type w)))))
                           transformed)))
        (should (eq 'word (cdr (assq 'contents obj))))))))

(ert-deftest composiphrase--pre-transformer-modifier-match ()
  "Test that pre-transformer modifier matching works correctly,
including matching on specific modifier values."
  (let* ((config
          `((verbs . ((move (count . 1))))
            (objects . ((word (default-verb . move))
                        (alt-word-1 (default-verb . move))
                        (alt-word-2 (default-verb . move))))
            (sentence-pre-transformers
             .
             (;; alternate=1 -> alt-word-1, alternate=2 -> alt-word-2
              ((,(lambda (v) t) word
                ((alternate 1))
                ,(lambda (sentence _params)
                   (mapcar (lambda (w)
                             (cond ((and w (eq 'object (cdr (assq 'word-type w))))
                                    '((word-type . object) (contents . alt-word-1)))
                                   ((and w (eq 'modifier (cdr (assq 'word-type w)))
                                         (eq 'alternate (cdr (assq 'parameter-name w))))
                                    nil)
                                   (t w)))
                           sentence)))
               (,(lambda (v) t) word
                ((alternate 2))
                ,(lambda (sentence _params)
                   (mapcar (lambda (w)
                             (cond ((and w (eq 'object (cdr (assq 'word-type w))))
                                    '((word-type . object) (contents . alt-word-2)))
                                   ((and w (eq 'modifier (cdr (assq 'word-type w)))
                                         (eq 'alternate (cdr (assq 'parameter-name w))))
                                    nil)
                                   (t w)))
                           sentence))))
              ))
            (match-table
             .
             ((move word () (move-word ()))
              (move alt-word-1 () (move-alt-word-1 ()))
              (move alt-word-2 () (move-alt-word-2 ()))))))
         ;; alternate=1 -> alt-word-1
         (sentence-alt1 (list '((word-type . verb) (contents . move))
                              '((word-type . object) (contents . word))
                              '((word-type . modifier) (parameter-name . alternate) (contents . 1))))
         ;; alternate=2 -> alt-word-2
         (sentence-alt2 (list '((word-type . verb) (contents . move))
                              '((word-type . object) (contents . word))
                              '((word-type . modifier) (parameter-name . alternate) (contents . 2))))
         ;; no alternate -> word (unchanged)
         (sentence-none (list '((word-type . verb) (contents . move))
                              '((word-type . object) (contents . word)))))
    (let ((t1 (composiphrase--apply-pre-transformers sentence-alt1 config)))
      (should (eq 'alt-word-1
                  (cdr (assq 'contents
                             (seq-find (lambda (w) (and w (eq 'object (cdr (assq 'word-type w)))))
                                       t1))))))
    (let ((t2 (composiphrase--apply-pre-transformers sentence-alt2 config)))
      (should (eq 'alt-word-2
                  (cdr (assq 'contents
                             (seq-find (lambda (w) (and w (eq 'object (cdr (assq 'word-type w)))))
                                       t2))))))
    (let ((t3 (composiphrase--apply-pre-transformers sentence-none config)))
      (should (eq 'word
                  (cdr (assq 'contents
                             (seq-find (lambda (w) (and w (eq 'object (cdr (assq 'word-type w)))))
                                       t3))))))))
