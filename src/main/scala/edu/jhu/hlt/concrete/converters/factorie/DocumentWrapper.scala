package edu.jhu.hlt.concrete.converters.factorie

import cc.factorie.app.nlp.Document
import edu.jhu.hlt.concrete.Concrete.{SentenceSegmentation, Communication, Section}
import scala.collection.JavaConverters._
import cc.factorie.{CategoricalValue, CategoricalDomain}
import cc.factorie._
import cc.factorie.app.nlp._

/**
 * @author John Sullivan, Tan Xu
 */
object MyPosDomain extends CategoricalDomain[String]
object MyNerDomain extends CategoricalDomain[String]
class MyPosLabel(val token:Token, targetValue:String) extends LabeledCategoricalVariable(targetValue) { def domain = MyPosDomain } 
class MyNerLabel(val token:Token, targetValue:String) extends LabeledCategoricalVariable(targetValue) { def domain = MyNerDomain }
class Lemmas(lemmas:String) extends StringVariable(lemmas)
class TokenId(id:Int=99999) extends IntegerVariable(id)
class SectionSegmentationTheory(theory:String="") extends StringVariable(theory)
class SentenceSegmentationTheory(theory:String="") extends StringVariable(theory)
class TokenizationTheory(theory:String="") extends StringVariable(theory)
class PosTagTheory(theory:String="") extends StringVariable(theory)
class NerTagTheory(theory:String="") extends StringVariable(theory)
class LemmasTheory(theory:String="") extends StringVariable(theory)

class DocumentWrapper(comm:Communication) extends Iterable[Document] {

  // Implement Iterable[Document]
  def iterator:Iterator[Document] = docs.toIterator

  private val docName:String = comm.getGuid.getCorpusName + ":" + comm.getGuid.getCommunicationId
  private val docString:String = comm.getText
  private var docs:Seq[Document] = Seq()
  
  private def newDocument[T<:StringVariable](prevDoc:Document, theory:T):Document ={
  	val doc = new Document(docString).setName(docName)
  	doc.attr+=new SectionSegmentationTheory
  	doc.attr+=new SentenceSegmentationTheory
  	doc.attr+=new TokenizationTheory
  	doc.attr+=new PosTagTheory
  	doc.attr+=new NerTagTheory
  	doc.attr+=new LemmasTheory
  	if(prevDoc ne null) 
  		copy[T](prevDoc, doc, theory)
  	docs :+= doc
  	doc
  }
  
  private def copy[T<:StringVariable](doc1:Document, doc2:Document, theory:T):Unit={
  	// copy doc1 to doc2
  	doc2.attr[SectionSegmentationTheory].set(doc1.attr[SectionSegmentationTheory].value)(null)
  	doc2.attr[SentenceSegmentationTheory].set(doc1.attr[SentenceSegmentationTheory].value)(null)
  	doc2.attr[TokenizationTheory].set(doc1.attr[TokenizationTheory].value)(null)
  	doc2.attr[PosTagTheory].set(doc1.attr[PosTagTheory].value)(null)
  	doc2.attr[NerTagTheory].set(doc1.attr[NerTagTheory].value)(null)
  	doc2.attr[LemmasTheory].set(doc1.attr[LemmasTheory].value)(null)
  	copySentence(doc1, doc2)
  	theory match{ 
  		case x:PosTagTheory => 
  		case _ => copyToken(doc1,doc2)
  	}
  }
  
  private def copySentence(doc1:Document, doc2:Document):Unit={
  	doc1.sentences.foreach(sent=>{new Sentence(doc2, sent.start, sent.length)})
  }
  
  private def copyToken(doc1:Document, doc2:Document):Unit={
  	doc1.foreach(tok1=>{
  		val tok2 = new Token(doc2, tok1.string)
  		// copy TOKEN attrs
  		tok2.attr+=new MyPosLabel(tok2, tok1.attr[MyPosLabel].categoryValue)
  		tok2.attr+=new MyNerLabel(tok2, tok1.attr[MyNerLabel].categoryValue)
  		tok2.attr+=new Lemmas(tok1.attr[Lemmas].value)
  	})
  }
  
  private def setTheory[T<:StringVariable](theory:T):Document={
  	val classType = theory.getClass()
  	val str = theory.value
  	var doc:Document = null
  	val curTheoryValue = docs.last.attr.apply(classType).value
  	if(curTheoryValue!="" && curTheoryValue!=str) 
  		doc = newDocument[T](docs.last, theory)
  	else doc = docs.last
  	doc.attr.apply(classType).set(str)(null)
  	doc
  }
  
  
  var document = newDocument(null, null)
  var theory = ""
  comm.getSectionSegmentationList().asScala.foreach(ss=>{
  	theory = ss.getMetadata().getTool()
  	document = setTheory[SectionSegmentationTheory](new SectionSegmentationTheory(theory))
  	ss.getSectionList.asScala.foreach(sec=>{
  		sec.getSentenceSegmentationList.asScala.foreach(sentSeg=>{
  			theory = sentSeg.getMetadata().getTool()
  			document = setTheory[SentenceSegmentationTheory](new SentenceSegmentationTheory(theory))
  			sentSeg.getSentenceList.asScala.foreach(sent=>{
  				new Sentence(document)
  				sent.getTokenizationList.asScala.foreach(tokenization=>{
  					theory = tokenization.getMetadata().getTool()
  					document = setTheory[TokenizationTheory](new TokenizationTheory(theory))
  					val sentence = document.sentences.last
  					// TOKEN
  					tokenization.getTokenList.asScala.foreach(tok=>{
  						val token = new Token(sentence, tok.getText())
  						token.attr+=new TokenId(tok.getTokenId)
  					})
  					// POS TAG
  					tokenization.getPosTagsList.asScala.foreach(tokTag=>{
  						theory = tokTag.getMetadata().getTool()
  						document = setTheory[PosTagTheory](new PosTagTheory(theory))
  						tokTag.getTaggedTokenList.asScala.foreach(posTok=>{
  							val posTokId = posTok.getTokenId
  							val posTokTag = posTok.getTag()
  							sentence.tokens.filter(token=>token.attr[TokenId].value==posTokId)
  								.foreach(token=>token.attr+=new MyPosLabel(token, posTokTag))
  						})
  					})
  					
  					// NER TAG
  					tokenization.getNerTagsList.asScala.foreach(nerTag=>{
  						theory = nerTag.getMetadata().getTool()
  						document = setTheory[NerTagTheory](new NerTagTheory(theory))
  						nerTag.getTaggedTokenList.asScala.foreach(nerTok=>{
  							val nerTokId = nerTok.getTokenId()
  							val nerTokTag = nerTok.getTag()
  							sentence.tokens.filter(token=>token.attr[TokenId].value==nerTokId)
  								.foreach(token=>token.attr+=new MyNerLabel(token, nerTokTag))
  						})
  						
  					})
  					// LEMMAS
  					tokenization.getLemmasList.asScala.foreach(lemmas=>{
  						theory = lemmas.getMetadata().getTool()
  						document = setTheory[LemmasTheory](new LemmasTheory(theory))
  						lemmas.getTaggedTokenList.asScala.foreach(lemTok=>{
  							val lemTokId = lemTok.getTokenId()
  							val lemTokTag = lemTok.getTag()
  							sentence.tokens.filter(token=>token.attr[TokenId].value==lemTokId)
  								.foreach(token=>token.attr+=new Lemmas(lemTokTag))
  						})
  					})
  					
  					// DEPENDENCE PARSE see cc.factorie.app.nlp.parse.ParseLabel.scala
  				})
  			})
  		})
  	})
  })
}
