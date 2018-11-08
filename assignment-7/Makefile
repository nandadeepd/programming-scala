# Makefile for CS558 Assignment 7
#

SC = scalac
TestFlag = -cp $(HOME)/scalatest-app_2.12-3.0.5.jar

.SUFFIXES: .scala .class

.PRECIOUS: %.scala

.scala.class:
	$(SC) $*.scala

sexpr:	SExprLibrary.class 

el3:	sexpr EL3.class
int3: 	sexpr el3 Interp3.class

testint3: sexpr el3 int3 TestInterp3.scala
	$(SC) $(TestFlag) SExprLibrary.scala EL3.scala \
           Interp3.scala TestInterp3.scala

testnew: sexpr el3 int3 TestNew.scala
	$(SC) $(TestFlag) SExprLibrary.scala EL3.scala \
           Interp3.scala TestNew.scala

clean:	
	rm *.class
