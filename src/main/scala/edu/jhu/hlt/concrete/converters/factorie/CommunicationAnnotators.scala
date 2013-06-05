package edu.jhu.hlt.concrete.converters.factorie

import cc.factorie.app.nlp.{Document, DocumentAnnotator, Token, Sentence}
import edu.jhu.hlt.concrete.Concrete.Communication
import cc.factorie.app.nlp.pos.PTBPosLabel
import cc.factorie.app.nlp.parse.ParseTree

/**
 * @author John Sullivan
 */
abstract class CommunicationAnnotator(comm:Communication) extends DocumentAnnotator {

}

class CommunicationSegmenter(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = null

  def postAttrs: Iterable[Class[_]] = Vector[Class[_]](classOf[Sentence], classOf[Token])
}
class CommunicationPOSTagger(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = null

  def postAttrs: Iterable[Class[_]] = Seq(classOf[PTBPosLabel])
}
class CommunicationLemmatizer(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = null

  def postAttrs: Iterable[Class[_]] = null //todo Communication Lemma class?
}
class CommunicationNERTagger(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = null

  def postAttrs: Iterable[Class[_]] = //todo Communication NER class?
}
class CommunicationDependencyParser(comm:Communication) extends CommunicationAnnotator(comm){
  def process1(document: Document): Document = null

  def prereqAttrs: Iterable[Class[_]] = null

  def postAttrs: Iterable[Class[_]] = Seq(classOf[ParseTree])
}
