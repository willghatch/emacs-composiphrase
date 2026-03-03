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
