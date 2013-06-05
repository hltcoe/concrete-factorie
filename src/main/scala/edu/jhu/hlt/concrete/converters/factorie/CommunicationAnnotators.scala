package edu.jhu.hlt.concrete.converters.factorie

import cc.factorie.app.nlp.{Document, DocumentAnnotator}
import edu.jhu.hlt.concrete.Concrete.Communication

/**
 * @author John Sullivan
 */
abstract class CommunicationAnnotator(comm:Communication) extends DocumentAnnotator {

}

class CommunicationSegmenter(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = null

  def postAttrs: Iterable[Class[_]] = null
}
class CommunicationTokenizer(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = null

  def postAttrs: Iterable[Class[_]] = null
}
class CommunicationPOSTagger(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = null

  def postAttrs: Iterable[Class[_]] = null
}
class CommunicationLemmatizer(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = null

  def postAttrs: Iterable[Class[_]] = null
}
class CommunicationNERTagger(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = null

  def postAttrs: Iterable[Class[_]] = null
}
class CommunicationDependencyParser(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = null

  def postAttrs: Iterable[Class[_]] = null
}
