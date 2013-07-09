package edu.jhu.hlt.concrete.converters.factorie

import cc.factorie.app.nlp.Document
import edu.jhu.hlt.concrete.Concrete.Communication
import edu.jhu.hlt.concrete.converters.factorie.utils._
import edu.jhu.hlt.concrete.converters.factorie.utils.Factorie2ConcreteFactorie._
import cc.factorie.StringVariable

/**
 * @author John Sullivan, Tan Xu
 */
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
  	var documents = docs.filter(doc=>(doc.getTheory(classType).value=="" 
  								|| doc.getTheory(classType).value==str))
  	if(documents.isEmpty){
  		println("Adding document by new theory: "+theory)
  		documents = Seq(newDocument[T](docs.last, theory))
  	} 
  	documents.foreach(doc=>doc.getTheory(classType).set(str)(null))
  	documents
  }
  
  private def newDocument[T<:StringVariable](prevDoc:Document, theory:T):Document ={
  	//val doc = new Document(docString).setName(docName)
  	val doc = new Document().setName(docName)
  	doc.initAttrs
  	if(prevDoc ne null) doc.copy[T](prevDoc, theory)
  	add(doc)
  	doc
  }
  
  newDocument(null, null)
}
