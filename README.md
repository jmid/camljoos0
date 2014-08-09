Joos0 compiler
==============

This is a simple compiler for a small subset of the Java(tm)
programming language. The supported language, Joos0, is the smallest
of a hierarchy of Java 1.3 subsets. For more language details and an
overview of the hierarchy, see
[https://services.brics.dk/java/courseadmin/dOvs2012/pages/The+Joos+Languages].

The compiler emits Java bytecode in the Jasmin format. You'll need to
install the Jasmin assembler to turn the emitted .j files into .class
files. For more information on Jasmin, see [http://jasmin.sourceforge.net/].

The compiler was developed as part of (a previous version of) the
undergraduate compiler course at Aarhus University.


To build:
---------

    $ make


To run:
-------

Example 1:

    $ ./joos0 tests/IntegerToString.java 
    [...]
    $ jasmin tests/IntegerToString.j
    $ java -classpath . IntegerToString
    Enter a number: 42
    In base 8 that is: 52

(hence the program tests/IntegerToString.java compiles with the Joos0
compiler, the output can be assembled with Jasmin, and the resulting
output run on the JVM)



Example 2:

    $ ./joos0 tests/Cons.java tests/ConsMain.java
    [...]
    $ jasmin tests/Cons.j tests/ConsMain.j
    $ java -classpath . ConsMain
    truefalse

(hence the compiler supports programs that span multiple files)



Example 3:

    $ ./joos0 tests/ClassName2.java 
    [...]
    Error at tests/ClassName2.java: line 3, col 11
        public A() throws Exception { }
    
    Constructor must have the same name as its enclosing class

(hence the program is properly rejected by the compiler with an error message)
