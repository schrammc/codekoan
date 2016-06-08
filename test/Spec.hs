{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import Data.Text (Text)

import Thesis.CodeAnalysis.StackoverflowBodyParser

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Thesis.Tokenizer" $ do
    it "removes linebreaks from" $ do
      (length $ readCodeFromHTMLPost testString) `shouldBe` 2


testString :: Text
testString = "<p>In the first case, you have a widening conversion happening. This can be see when runinng the \"javap\" utility program (included w/ the JDK), on the compiled class:</p>\r\n\r\n<pre><code>public static void main(java.lang.String[]);<br>  Code:<br>   0:   iconst_ 5<br>   1:   istore_ 1<br>   2:   iload_ 1<br>   3:   i2l<br>   4:   invokestatic    #6; //Method hello:(J)V<br>   7:   return<br><br>}<br></code></pre>\r\n\r\n<p>Clearly, you see the I2L, which is the mnemonic for the widening Integer-To-Long bytecode instruction. See reference <a href=\"http://java.sun.com/docs/books/jvms/second_edition/html/Instructions2.doc6.html\">here</a>.</p>\r\n\r\n<p>And in the other case, replacing the \"long x\" with the object \"Long x\" signature, you'll have this code in the main method:</p>\r\n\r\n<pre><code>public static void main(java.lang.String[]);<br>  Code:<br>   0:   iconst_ 5<br>   1:   istore_ 1<br>   2:   iload_ 1<br>   3:   invokestatic    #6; //Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;<br>   6:   invokestatic    #7; //Method hello:(Ljava/lang/Integer;)V<br>   9:   return<br><br>}<br></code></pre>\r\n\r\n<p>So you see the compiler has created the instruction Integer.valueOf(int), to box the primitive inside the wrapper.</p>"

