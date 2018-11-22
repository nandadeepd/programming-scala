# Makefile for CS558 Assignment 9
#

SC = scalac
TestFlag = -cp $(HOME)/scalatest-app_2.12-3.0.5.jar

.SUFFIXES: .scala .class

.PRECIOUS: %.scala

.scala.class:
	$(SC) $*.scala

sexpr:	SExprLibrary.class 

el4:	sexpr EL4.class

check: 	sexpr el4 Check.class

testcheck: sexpr el4 check TestCheck.scala
	$(SC) $(TestFlag) SExprLibrary.scala EL4.scala \
           Check.scala TestCheck.scala

clean:	
	rm *.class
