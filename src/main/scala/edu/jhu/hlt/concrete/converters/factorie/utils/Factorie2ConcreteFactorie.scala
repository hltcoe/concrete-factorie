package edu.jhu.hlt.concrete.converters.factorie.utils

import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.Sentence
import cc.factorie.app.nlp.Token
import cc.factorie._
import cc.factorie.app.nlp.lemma.TokenLemma

/*
 * @author Tan Xu
 * Implicit convert from factorie data type to customized data type, in order to 
 * add more functions
 */
object Factorie2ConcreteFactorie {
	
	implicit def document2ConcreteDocument(document:Document)=new ConcreteDocument(document)
	implicit def sentence2ConcreteSentence(sentence:Sentence)=new ConcreteSentence(sentence)
	implicit def token2ConcreteToken(token:Token)=new ConcreteToken(token)
	
	class ConcreteDocument(doc:Document){
		def initAttrs ={
			doc.attr+=new SectionSegmentationTheory
			doc.attr+=new SentenceSegmentationTheory
			doc.attr+=new TokenizationTheory
			doc.attr+=new PosTagTheory
			doc.attr+=new NerTagTheory
			doc.attr+=new LemmasTheory
		}
		def sectionSegmentationTheory = doc.attr[SectionSegmentationTheory]
		def sentenceSegmentationTheory = doc.attr[SentenceSegmentationTheory]
		def tokenizationTheory = doc.attr[TokenizationTheory]
		def posTagTheory = doc.attr[PosTagTheory]
		def nerTagTheory = doc.attr[NerTagTheory]
		def lemmasTheory = doc.attr[LemmasTheory]
		def getTheory[C<:AnyRef](theory:java.lang.Class[C]) = doc.attr.apply(theory)
		def copy[T<:StringVariable](doc1:Document, theory:T)={
			// copy doc1 to doc2
			doc.sectionSegmentationTheory.set(doc1.sectionSegmentationTheory.value)(null)
			doc.sentenceSegmentationTheory.set(doc1.sentenceSegmentationTheory.value)(null)
			doc.tokenizationTheory.set(doc1.tokenizationTheory.value)(null)
			doc.posTagTheory.set(doc1.posTagTheory.value)(null)
			doc.nerTagTheory.set(doc1.nerTagTheory.value)(null)
			doc.lemmasTheory.set(doc1.lemmasTheory.value)(null)
			doc1.sentences.foreach(sent=>{new Sentence(doc, sent.start, sent.length)})
			theory match{ 
				case x:PosTagTheory => 
				case _ => doc1.foreach(
						tok1=>{val tok2 = new Token(doc, tok1.string); tok2.copy(tok1)})
			}
		}
	}
	
	class ConcreteSentence(sentence:Sentence){}
	class ConcreteToken(token:Token){
		def setId(id:Int) = token.attr+=new TokenId(id)
		def getId = token.attr[TokenId]
		def initPosLabel(value:String) = token.attr+=new MyPosLabel(token, value)
		def initNerLabel(value:String) = token.attr+=new MyNerLabel(token, value)
		def initTokenLemma(value:String) = token.attr+=new TokenLemma(token, value)
		def myPosLabel = token.attr[MyPosLabel]
		def myNerLabel = token.attr[MyNerLabel]
		def copy(tok1:Token)={
			// copy TOKEN attrs
			token.setId(tok1.getId.value)
			token.initPosLabel(tok1.myPosLabel.categoryValue)
			token.initNerLabel(tok1.myNerLabel.categoryValue)
			token.initTokenLemma(tok1.lemma.value)
		}
	}
}

/**Concrete token attributes**/
object MyPosDomain extends CategoricalDomain[String]
object MyNerDomain extends CategoricalDomain[String]
class MyPosLabel(val token:Token, targetValue:String) extends LabeledCategoricalVariable(targetValue) { def domain = MyPosDomain } 
class MyNerLabel(val token:Token, targetValue:String) extends LabeledCategoricalVariable(targetValue) { def domain = MyNerDomain }
class TokenId(id:Int=99999) extends IntegerVariable(id)

/**Concrete document attributes**/
class SectionSegmentationTheory(theory:String="") extends StringVariable(theory)
class SentenceSegmentationTheory(theory:String="") extends StringVariable(theory)
class TokenizationTheory(theory:String="") extends StringVariable(theory)
class PosTagTheory(theory:String="") extends StringVariable(theory)
class NerTagTheory(theory:String="") extends StringVariable(theory)
class LemmasTheory(theory:String="") extends StringVariable(theory)