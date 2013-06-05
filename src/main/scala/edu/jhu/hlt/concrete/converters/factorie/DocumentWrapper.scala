package edu.jhu.hlt.concrete.converters.factorie

import cc.factorie.app.nlp.Document
import edu.jhu.hlt.concrete.Concrete.{SentenceSegmentation, Communication, Section}
import scala.collection.JavaConverters._
import cc.factorie.{CategoricalValue, CategoricalDomain}
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.PTBPosLabel

/**
 * @author John Sullivan, Tan Xu
 */
class TokenId(id:Int=99999) extends IntegerVariable(id)
class SectionSegmentationTheory(theory:String="") extends StringVariable(theory)
class SentenceSegmentationTheory(theory:String="") extends StringVariable(theory)
class TokenizationTheory(theory:String="") extends StringVariable(theory)
class PosTagTheory(theory:String="") extends StringVariable(theory)

class DocumentWrapper(comm:Communication) extends Iterable[Document] {

  // Implement Iterable[Document]
  def iterator:Iterator[Document] = docs.toIterator

  private val docName:String = comm.getGuid.getCorpusName + ":" + comm.getGuid.getCommunicationId
  private val docString:String = comm.getText
  private var docs:Seq[Document] = Seq()
  
  private def newDocument(prevDoc:Document):Document ={
  	val doc = new Document(docString).setName(docName)
  	if(prevDoc eq null){
  		doc.attr+=new SectionSegmentationTheory
  		doc.attr+=new SentenceSegmentationTheory
  		doc.attr+=new TokenizationTheory
  		doc.attr+=new PosTagTheory
  	}else{
  		copy(prevDoc, doc)
  	}
  	docs :+= doc
  	doc
  }
  
  private def copy(doc1:Document, doc2:Document):Unit={
  	// copy doc1 to doc2
  	// not finished
  }
  
  private def setTheory[T<:StringVariable](theory:T):Document={
  	val classType = theory.getClass()
  	val str = theory.value
  	var doc:Document = null
  	if(docs.last.attr.apply(classType).value !=str) doc = newDocument(docs.last)
  	else doc = docs.last
  	doc.attr.apply(classType).set(str)(null)
  	doc
  }
  
  
  var document = newDocument(null)
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
  						tokTag.getTaggedTokenList.asScala.foreach(tagTok=>{
  							val tagTokId = tagTok.getTokenId
  							val tagTokTag = tagTok.getTag()
  							sentence.tokens.filter(token=>token.attr[TokenId].value==tagTokId)
  								.foreach(token=>token.attr+=new PTBPosLabel(token, tagTok.getTag))
  						})
  					})
  					
  					// NER TAG
  					
  					// LEMMAS
  					
  				})
  			})
  		})
  	})
  })
}
