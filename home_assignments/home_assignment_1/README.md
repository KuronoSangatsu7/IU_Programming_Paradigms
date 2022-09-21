## Programming Paradigms - F22 - Homework Assignment No.1
##### Jaffar Totanji - SD-01

### Code Overview: 

The first thing one might notice about the program is the sheer size of it, which may seem daunting at first but rest assured, it's not 800+ lines of pure logic. The code was simply a linear solution to the problem statement, that is, for each part of the assignment you will find the corresponding function implemented (with 2 exceptions to be mentioned later) along with helper functions to make the code more readable/maintainable. Any updates to previous functions (e.g. derivative, simplify) were implemented as new functions while keeping the original functions in the code, which contributed heavily to the size of the program. 

The program also has what could be called Unit Tests. Unless a function is overly simple, it is likely to be followed by some tests to present its usage and correctness. All the tests and results are printed to standard output.

Every function required by the problem statement was implemented with the same name per the problem statement, with a couple of exceptions:

- 1.6: Update `derivative` and `simplify`, the functions were implemented again to fulfill the new requirements with names `derivative-extended` and `simplify-extended` while keeping the old `derivative` and `simplify` functions untouched. All corresponding helper functions affected by the change were also implemented again and suffixed with `-extended`

- 1.7: Same thing as 1.6, except the functions/helpers were suffixed with `-final`.

### Code Structure:

The structure is pretty straightforward, there are comments with the number of the exercise (e.g. 1.1, 1.5, 1.7, etc.) possibly followed by helper function(s) that aid the main function and contribute to the readabiliy of the code, then the solution to the exercise which is the main function required by the exercise.

Each function is also preceded by a comment describing the main functionality of the function.

### Usage:

The code can be used in one of two ways:

- It can either be simply copy-pasted into DrRacket to be run as a standalone program, for which the unit tests should be somewhat sufficient to display the general functionality of the program.

- Or it can be copy-pasted into DrRacket and extended by further testing for any function in the program. However, please note that not all funcitons were implemented to be used by a user, in which case the user might be faced with unexpected behaviour. The user can always refer the unit tests succeding a function to learn about how it can be used if they wish to use such functions for different purposes. The functions that were meant to be used by the user are the ones that I consider to be the "main" functions which display the general purpose of the program, namely: `derivative-final`, `simplify-final`, `gradient` and `variables-of`. These functions will raise errors if called with improper input syntax. The validity of the input syntax can be checked with the function `verify-expression-integrity` prior to using any of the above functions, or any other function which accepts input of the same form to avoid errors.

### Author Contact Info:

    Jaffar Totanji - SD-01
    Email: j.totanji@innopolis.university
    Telegram: @KuroHata7