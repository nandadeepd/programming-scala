# Makefile for CS558 Assignment 6
#

SC = scalac
TestFlag = -cp $(HOME)/scalatest-app_2.12-3.0.5.jar

.SUFFIXES: .scala .class

.PRECIOUS: %.scala

.scala.class:
	$(SC) $*.scala

sexpr:	SExprLibrary.class 

el2:	sexpr EL2.class
int2: 	sexpr el2 Interp2.class

testint2: sexpr el2 int2 TestInterp2.scala
	$(SC) $(TestFlag) SExprLibrary.scala EL2.scala \
           Interp2.scala TestInterp2.scala

clean:	
	rm *.class
