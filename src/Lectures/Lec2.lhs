---
fulltitle: Structured Data and Lists
date: September 6, 2017
---

> {-# OPTIONS -fwarn-tabs -fno-warn-type-defaults #-}

> module Lec2 where
> import Data.Char
> import Test.HUnit

Structured Data
===============


Tuples
------

        (A1, ..., An)

Ordered sequence of values of type `A1`, .. `An`

> t1 = ('a', 5)          :: (Char, Int)               -- the spacing doesn't matter
> t2 = ('a', 5.2, 7)     :: (Char, Double, Int)       -- but it is pretty to line
> t3 = ((7, 5.2), True)  :: ((Int, Double), Bool)     -- things up in your code

The structure must match the type.



Extracting values from tuples
-----------------------------

*Pattern Matching* extracts values from tuples.

A function that takes a tuple as an argument *looks* like it has multiple
arguments, but in reality, it has just one. We use a *pattern* to name the
three components of the tuple for use in the function.


> pat :: (Int, Int, Int) -> Int
> pat (x, y, z) = x * (y + z)




We can put *anything* in a tuple
--------------------------------

We can have tuples of tuples. These three values have three different types.

> tup1 = ((1,2),3) :: ((Int,Int),Int)   -- a pair of a pair and a number
> tup2 = (1,(2,3)) :: (Int,(Int,Int))  -- a pair of a number and a pair
> tup3 = (1, 2, 3) :: (Int, Int, Int)  -- a three-tuple


Note that the pattern that names the variables
must match the structure *exactly*.

> pat2 :: ((Int,Int),Int) -> Int
> pat2 ((x, y), z) = x * (y + z)

> pat3 :: (Int, (Int, Int)) -> Int
> pat3 (x, (y, z)) = x * (y + z)


We can also stick IO actions in a pair.

> act2 :: (IO (), IO ())
> act2 = (putStr "Hello", putStr "Hello")


This doesn't actually run both actions, it just creates a pair holding
two IO computations.

Compare the difference between these definitions in ghci.

> runAct2 :: IO ()
> runAct2 = do
>    let (x, y) = act2     -- pattern match in `do` sequences using `let`
>    x                     -- run the first action
>    y                     -- then run the second


> runAct2' :: IO ()
> runAct2' = do
>    let (x, y) = act2     -- pattern match
>    y                     -- run the second action
>    x                     -- then run the first


> runAct2'' :: IO ()
> runAct2'' = do
>    let (x, y) = act2     -- pattern match
>    x                     -- run the first action
>    x                     -- then run it again!







Optional values
---------------

        Maybe A

Either a value of type `A`, or else nothing.

> m1 :: Maybe Int
> m1 = Just 2

> m2 :: Maybe Int
> m2 = Nothing


There is no 'null' in Haskell. Yay!



Extracting values from 'Maybe's
--------------------------------

Pattern Matching extracts values from maybes; need a pattern for each
case.


> pat'' :: Maybe Int -> Int
> pat'' (Just x) = x
> pat'' Nothing  = 2


Patterns can be nested, too.

> jn :: Maybe (Maybe a) -> Maybe a
> jn (Just (Just x)) = Just x
> jn (Just Nothing) = Nothing
> jn Nothing = Nothing

See if you can come up with a slightly simpler way to write `jn` using two
patterns instead of three.

> jn' :: Maybe (Maybe a) -> Maybe a
> jn' = undefined



'Maybe' is useful for partial functions
---------------------------------------

> location :: String -> Maybe String
> location "cis501" = Just "Wu & Chen"
> location "cis502" = Just "Heilmeier"
> location "cis520" = Just "Wu & Chen"
> location "cis552" = Just "3401 Walnut: 401B"
> location _        = Nothing    -- wildcard pattern, matches anything


Lists
=====

       [A]

A list is a sequence of values of the same type. There is no limit to
the number of values that can be stored in a list. We notate lists as
a sequence of comma-separated values inside square brackets.

> l1 :: [Double]
> l1 = [1.0,2.0,3.0,4.0]

> l2 :: [Int]
> l2 = undefined -- make a list of numbers

Lists can contain structured data...

> l3 :: [(Int,Bool)]
> l3 = [ (1,True), (2, False) ]

...and can be nested:

> l4 :: [[Int]]
> l4 = undefined

List elements *must* have the same type.

> -- l5 :: [Int]
> -- l5 = [ 1 , True ]  -- doesn't type check

(see the type error that results when you uncomment the definition above and reload the file in ghci.)

The empty list is written `[]` and pronounced "nil".

> l6 :: [a]
> l6 = []

*Note*: `String` is just another name for a list of characters (`[Char]`).

> l7 :: String
> l7 = ['h','e','l','l','o',' ','5','5','2','!']

What happens when you print l7?

       ghci> l7
       undefined -- fill in with correct answer


"Cons"tructing Lists
---------------------

The infix operator `:` constructs a new list, by adding a new element to the
front of an existing list. (Note, the existing list is not modified.)
We call this operator `cons`:

(Note that the `a` in the type of `cons` means that this function works for
lists containing *any* type of element. In otherwords, we say that this function
is *polymorphic*. More on this later.)

> cons :: a -> [a] -> [a]
> cons = (:)

> c1 :: [Char]
> c1 = 'a' : ['b', 'c']

> c2 :: [Int]
> c2 = 1 : []

Try printing `c1` and `c2`

       ghci> c1
       undefined
       ghci> c2
       undefined


> -- undefined: fill in the type of c3
> c3 = [] : []



Syntactic Sugar
---------------

GHC views the notation

       [x1,x2, .. , xn]

 as short for

       x1 : x2 : .. : xn : []

This means that we can think of lists as a sequence of cons'ed elements,
ending with nil.  For example,


        [1,2,3,4]  and  1 : 2 : 3 : 4

are the same list:

        ghci> [1,2,3,4] == 1:2:3:4:[]
        True


Function practice: List Generation
----------------------------------

Problem: Write a function that, given an argument `x` and a number `n`, returns
a list containing `n` copies of `x`.

Step 1: Define test cases for the function.

We're using HUnit, a library for defining unit tests in Haskell.  The (~?=)
operator constructs a unit `Test` by comparing the actual result (first
argument) with an expected result (second argument). Haskell is lazy, so
these definitions *create* tests, but don't actually run them yet.


> testClone1, testClone2, testClone3 :: Test
> testClone1 = clone 'a' 4 ~?= ['a','a','a','a']
> testClone2 = clone 'a' 0 ~?= []
> testClone3 = clone 1.1 3 ~?= [1.1, 1.1, 1.1]
> testClone4 = clone 'a' (-1) ~?= []

Step 2: Define the type of the function

This function replicates any type of value, so the type of the first argument
is polymorphic.

> clone :: a -> Int -> [a]


Step 3: Implement the function

We implement this function by recursion on the integer argument.

> clone x n = if n<=0 then [] else x : clone x (n-1)


Step 4: Run the tests

The HUnit function `runTestTT` actually runs a given unit test and prints
its result to the standard output stream. (That is why its result type is `IO
Counts`. The IO in the type means that this computation does IO.)

> cl1, cl2, cl3 :: IO Counts
> cl1 = runTestTT testClone1
> cl2 = runTestTT testClone2
> cl3 = runTestTT testClone3
> cl4 = runTestTT testClone4

or

> cls :: IO Counts
> cls = runTestTT (TestList [ testClone1, testClone2, testClone3, testClone4 ])


You can run the tests by evaluating the definition `cls` at the ghci prompt.

        ghci> cls
        Cases: 4  Tried: 4  Errors: 0  Failures: 0
        Counts {cases = 4, tried = 4, errors = 0, failures = 0}


Function practice
-----------------

Define a function that, given two integers `i` and `j`, returns a list
containing all of the numbers at least as big as `i` but no bigger than `j`, in
order.

Step 1: Define test cases

> testRange :: Test
> testRange = TestList [ range 3  6  ~?= [3,4,5,6],
>                        range 42 42 ~?= [42],
>                        range 10 5  ~?= [] ]


Step 2: Declare the type

> range :: Int -> Int -> [Int]


Step 3: Define the function

> range i j = undefined


Step 4: run tests

> runRTests :: IO Counts
> runRTests = runTestTT testRange




Pattern matching with lists
---------------------------

The examples so far have *constructed* various lists. Of course, sometimes
we would like to write functions that *use* lists. We can use a list by
pattern matching...

> isHi :: String -> Bool
> isHi ['H','i'] = True
> isHi _ = False

If we have a list of characters, we can use string constants as patterns too.

> isGreeting :: String -> Bool
> isGreeting "Hi" = True
> isGreeting "Hello" = True
> isGreeting "Bonjour" = True
> isGreeting "Guten Tag" = True
> isGreeting _ = False

We can also work with lists more abstractly, for example determining if we have
a list of length one...

> isSingleton :: [a] -> Bool
> isSingleton [_] = True
> isSingleton _   = False

...or of length greater than two.

> isLong :: [a] -> Bool
> isLong = undefined

> testIsLong :: Test
> testIsLong = TestList [ not (isLong [])   ~? "nil",    -- can convert booleans to tests by naming them via `~?`
>                         not (isLong "a")  ~? "one",
>                         not (isLong "ab") ~? "two",
>                         isLong "abc"      ~? "three" ]

Function practice: List Recursion
------------------------------

Define a function, called listAdd, that, given a list of Ints returns
their sum.

Step 1: define test cases

> listAddTests :: Test
> listAddTests = TestList [ listAdd [1,2,3] ~?= 6,
>                           listAdd [] ~?= 0 ]


Step 2: define function signature

> listAdd :: [Int] -> Int


Step 3: implementation

(using pattern matching to define the function by
case analysis.)

> listAdd []     = 0
> listAdd (x:xs) = x + listAdd xs


Step 4: run the tests

> runLATests :: IO Counts
> runLATests = runTestTT listAddTests


Note that listAdd follows a general pattern of working with lists called *list
recursion*.  We can define lists as follows:

  A list is either
     []      -- the empty list
     x : xs  -- or, an element x cons'ed onto another list xs

This is a recursive definition, as we are defining lists in terms of
themselves.  Recursive functions that work with lists will follow the
pattern of this definition:

     f :: [a] -> ...
     f [] =  ...       -- case for the empty list
     f (x : xs) = ...  -- case for a nonempty list, will use `f xs`
                       -- recursively somehow.




Function practice: List transformation
------------------------------------

Define a function, called `listIncr`, that, given a list of ints,
returns a new list where each number has been incremented.

Step 1: write test case(s)

> listIncrTests :: Test
> listIncrTests =
>  TestList [ listIncr [1,2,3] ~?= [2,3,4],
>             listIncr [42]    ~?= [43] ]

Step 2: write function type

> listIncr :: [Int] -> [Int]

Step 3: define the function

> listIncr = undefined

Step 4: run the tests

> runLITests :: IO Counts
> runLITests = runTestTT listIncrTests

Step 5: refactor, if necessary












Acknowledgements: this lecture from cse 230 [1], which itself was
inspired by a previous version of CIS 552.

[1]: http://cseweb.ucsd.edu/classes/wi11/cse230/lectures.html
