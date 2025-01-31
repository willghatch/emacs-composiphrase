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

