package edu.jhu.hlt.concrete.converters.factorie

import cc.factorie.app.nlp.Document
import edu.jhu.hlt.concrete.Concrete.{SentenceSegmentation, Communication, Section}
import scala.collection.JavaConverters._
import cc.factorie.{CategoricalValue, CategoricalDomain}
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.lemma.TokenLemma
import cc.factorie.app.nlp.ner.ChainNerLabel

/**
 * @author John Sullivan, Tan Xu
 */
object MyPosDomain extends CategoricalDomain[String]
object MyNerDomain extends CategoricalDomain[String]
class MyPosLabel(val token:Token, targetValue:String) extends LabeledCategoricalVariable(targetValue) { def domain = MyPosDomain } 
class MyNerLabel(val token:Token, targetValue:String) extends LabeledCategoricalVariable(targetValue) { def domain = MyNerDomain }
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
  //private val docString:String = comm.getText
  private var docs:Seq[Document] = Seq()
  
  def add(doc:Document) = docs:+=doc
  
  def setTheory[T<:StringVariable](theory:T):Seq[Document]={
  	val classType = theory.getClass()
  	val str = theory.value
  	var documents = docs.filter(doc=>(doc.attr.apply(classType).value=="" 
  								|| doc.attr.apply(classType).value==str))
  	if(documents.isEmpty){
  		println("Adding document by new theory: "+theory)
  		documents = Seq(newDocument[T](docs.last, theory))
  	} 
  	documents.foreach(doc=>doc.attr.apply(classType).set(str)(null))
  	documents
  }
  
  private def newDocument[T<:StringVariable](prevDoc:Document, theory:T):Document ={
  	//val doc = new Document(docString).setName(docName)
  	val doc = new Document().setName(docName)
  	doc.attr+=new SectionSegmentationTheory
  	doc.attr+=new SentenceSegmentationTheory
  	doc.attr+=new TokenizationTheory
  	doc.attr+=new PosTagTheory
  	doc.attr+=new NerTagTheory
  	doc.attr+=new LemmasTheory
  	if(prevDoc ne null) 
  		copy[T](prevDoc, doc, theory)
  	add(doc)
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
  		tok2.attr+=new CommNerLabel(tok2, tok1.attr[MyNerLabel].categoryValue)
  		tok2.attr+=new TokenLemma(tok2, tok1.lemma.value)
  	})
  }
  
  newDocument(null, null)
}
