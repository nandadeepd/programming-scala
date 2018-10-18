# Makefile for CS558 Assignment 3
#

SC = scalac
TestFlag = -cp $(HOME)/scalatest-app_2.12-3.0.5.jar

.SUFFIXES: .scala .class

.PRECIOUS: %.scala

.scala.class:
	$(SC) $*.scala

sexpr:	SExprLibrary.class 

el1:	sexpr EL1.class
int1: 	sexpr el1 Interp1.class
int1b: 	sexpr el1 Interp1b.class

testint1: sexpr el1 int1 TestInterp1.scala
	$(SC) $(TestFlag) SExprLibrary.scala EL1.scala \
           Interp1.scala TestInterp1.scala

testint1b: sexpr el1 int1b TestInterp1b.scala
	$(SC) $(TestFlag) SExprLibrary.scala EL1.scala \
           Interp1b.scala TestInterp1b.scala

clean:	
	rm *.class
