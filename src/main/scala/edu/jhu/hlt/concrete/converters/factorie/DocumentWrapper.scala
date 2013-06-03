package edu.jhu.hlt.concrete.converters.factorie

import cc.factorie.app.nlp.Document
import edu.jhu.hlt.concrete.Concrete.{SentenceSegmentation, Communication, Section}
import scala.collection.JavaConverters._
import cc.factorie.{CategoricalValue, CategoricalDomain}

/**
 * @author John Sullivan
 */
class DocumentWrapper(comm:Communication) extends Iterable[Document] {

  // Implement Iterable[Document]
  def iterator:Iterator[Document] = docs.toIterator

  private val docName:String = comm.getGuid.getCorpusName + ":" + comm.getGuid.getCommunicationId
  private val docString:String = comm.getText

  private val docs:Seq[Document] = Seq(new Document(docString).setName(docName))

}
