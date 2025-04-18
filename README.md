# CMSC 430 Midterm 2

## Instructions

There are FIVE parts to this midterm. Each part has its own directory with a
README and additional files. Read the README for the instructions on what to do.

### Question Overview

  1. Honor pledge
  2. Hoax: implement `vector-append`
  3. Hoax: find bugs in `string->list`, `string-copy`, `string-append`
  4. Iniquity: explain implementation of default parameters
  5. Knock: implement `app` pattern
  6. Hustle: complete implementation of `append`

## Communications

If you have questions about the exam, post a _private_ question on Piazza. We
will disable public posts during the exam time to make sure this doesn't go
wrong. Old (public) Piazza posts will still be visible, and public posting will
be re-enabled after conclusion of the midterm period.

Answers to common clarifying questions will be posted by course staff in an
updating FAQ on Piazza.

You may not communicate with any person or entity outside of the course staff
about the midterm. Making use of any outside assistance during the midterm will
result in a referral to the OSC.

## Submission

To submit your work, first do `make submit.zip` from within the `m2/` directory
(the same directory where this README is located). Then upload the resulting
`submit.zip` file to Gradescope.

Unlike other assignments, the autograder will only give feedback on the basic
well-formedness of your submission. This means it will check that the layout is
correct and all necessary files are present, and it will ensure you have
uploaded syntactically valid Racket code. You will not receive any feedback
about the correctness of your solutions. Be sure to read the output you are
given to ensure your midterm was submitted correctly.

If you fail these well-formedness tests, we will not be able to grade your
submission. Passing these tests only means your submission is well-formed; your
actual grade will be computed after the deadline. **IT IS YOUR RESPONSIBILITY TO
UPLOAD A WELL-FORMED SUBMISSION.**

## Notes

  * In many places, we left `TODO` comments in the places where you ought to
    write code, but you should always start with each part's README.

  * We tried very hard to give sufficiently detailed explanations of the
    requirements for each part of the midterm. Although we will answer
    clarifying questions when you need help understanding what is asked of you,
    please do your best to look for the answers to your questions in the README
    files first.

  * You should not need to create any new files at any point, and we would
    prefer that you do not. You have been given places to write tests; we
    encourage you to use them.

  * Do not modify the names or signatures of any existing functions. Do not
    modify the exports of any existing files.

  * We **strongly** suggest reading all instructions thoroughly before
    beginning.

  * We also **strongly** recommend reading all modified code before making any
    changes.

  * The later languages have a lot of tests written, which means running tests
    via `raco test` might take a while. We prescribe patience, but if you'd
    prefer you can also just comment out or delete all the other tests --- but
    be careful not to introduce a regression!
# midterm-2
