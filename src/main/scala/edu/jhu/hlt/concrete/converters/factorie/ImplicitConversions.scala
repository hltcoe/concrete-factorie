package edu.jhu.hlt.concrete.converters.factorie

import java.util.UUID
import edu.jhu.hlt.concrete.Concrete.{Communication, UUID => ConUUID, CommunicationGUID, TextSpan, Token => ConToken,
                                      TokenTagging, Tokenization, TokenRef, Sentence => ConSentence,
                                      SentenceSegmentation, Section, SectionSegmentation, DependencyParse, AnnotationMetadata}
import edu.jhu.hlt.concrete.Concrete.DependencyParse.Dependency
import edu.jhu.hlt.concrete.Concrete.TokenTagging.TaggedToken
import cc.factorie.app.nlp.{Sentence, Document, Token}
import scala.collection.JavaConverters._
import cc.factorie.StringVariable

/**
 * @author John Sullivan, tanx
 * This object contains implicit conversions between Concrete Communications
 * and FactorIE Documents.
 */
object ImplicitConversions {

  /*
    Methods to convert Documents to Communications

   */

  private def getUUID:ConUUID = {
    val uuid = UUID.randomUUID
    ConUUID.newBuilder.setHigh(uuid.getMostSignificantBits).setLow(uuid.getLeastSignificantBits).build
  }

  private def factorieMetadata:AnnotationMetadata = AnnotationMetadata.newBuilder
    .setTool("FactorIE 1.0.0-M4")
    .setConfidence(1)
    .build

  implicit def Name2Guid(docName:String):CommunicationGUID = CommunicationGUID.newBuilder
    .setCorpusName(docName)
    .build

  implicit def Range2TextSpan(rng:Range):TextSpan = TextSpan.newBuilder
    .setStart(rng.start)
    .setEnd(rng.end)
    .build

  implicit def Fac2ConToken(facTok:Token):ConToken = ConToken.newBuilder
    .setTokenId(facTok.positionInSentence)
    .setText(facTok.string)
    .setTextSpan(facTok.stringStart to facTok.stringEnd)
    .build

  private def posTagging(tok:Token):Option[TaggedToken] = Option(tok.posLabel) match {
    case Some(posLabel) => Some(TaggedToken.newBuilder.setTokenId(tok.positionInSentence).setTag(posLabel.categoryValue).build)
    case _ => None
  }

  private def nerTagging(tok:Token):Option[TaggedToken] = Option(tok.nerLabel) match {
    case Some(nerLabel) => Some(TaggedToken.newBuilder.setTokenId(tok.positionInSentence).setTag(nerLabel.categoryValue).build)
    case _ => None
  }

  private def lemmaTagging(tok:Token):Option[TaggedToken] = Option(tok.lemma) match {
    case Some(lemma) => Some(TaggedToken.newBuilder.setTokenId(tok.positionInSentence).setTag(lemma.lemma).build)
    case _ => None
  }

  implicit def Tags2Tagging(toks:Iterable[Option[TaggedToken]]):TokenTagging = TokenTagging.newBuilder
    .setUuid(getUUID)
    .setMetadata(factorieMetadata)
    .addAllTaggedToken(toks.flatten.asJava)
    .build

  implicit def TokenIterable2Tokenization(tokens:Iterable[Token])(implicit uuid:ConUUID):Tokenization = {
    Tokenization.newBuilder
      .setUuid(uuid)
      .setMetadata(factorieMetadata)
      .setKind(Tokenization.Kind.TOKEN_LIST)
      .addAllToken((tokens map Fac2ConToken).asJava)
      .addPosTags(tokens map posTagging)
      .addNerTags(tokens map nerTagging)
      .addLemmas(tokens map lemmaTagging)
      .build
  }

  private def tokenToTokenRef(tok:Token, uuid:ConUUID):TokenRef = TokenRef.newBuilder
    .setTokenId(tok.position)
    .setTokenization(uuid)
    .build

  private def sentenceToDependencyParse(sent:Sentence, uuid:ConUUID):DependencyParse = DependencyParse.newBuilder
    .setUuid(getUUID)
    .setMetadata(factorieMetadata)
    .addAllDependency((sent.toIterable map {Token2Dependency(_,uuid)}).asJava)
    .build


  private def Token2Dependency(facTok:Token, uuid:ConUUID):Dependency = Option(facTok.parseParent) match {
      case Some(parentToken) => Dependency.newBuilder.setGov(tokenToTokenRef(parentToken, uuid)).setDep(tokenToTokenRef(facTok, uuid)).setEdgeType(facTok.parseLabel.categoryValue).build
      case None => Dependency.newBuilder.setDep(tokenToTokenRef(facTok, uuid)).setEdgeType(facTok.parseLabel.categoryValue).build
    }

  implicit def Fac2ConSentence(facSent:Sentence):ConSentence = {
    implicit val tokUUID:ConUUID = getUUID
    ConSentence.newBuilder
      .setUuid(getUUID)
      .setTextSpan(facSent.start to facSent.end)
      .addTokenization(facSent.toSeq)
      .addDependencyParse(sentenceToDependencyParse(facSent, tokUUID))
      .build
  }

  implicit def FacConSentenceSegmentation(sents:Iterable[Sentence]):SentenceSegmentation = SentenceSegmentation.newBuilder
    .setUuid(getUUID)
    .setMetadata(factorieMetadata)
    .addAllSentence((sents map Fac2ConSentence).asJava)
    .build

  private def Document2Communication(doc:Document):Communication = Communication.newBuilder
    .setUuid(getUUID)
    .setGuid(doc.name)
    .setText(doc.string)
    .addSectionSegmentation{
    SectionSegmentation.newBuilder // Factorie doesn't split documents into paragraphs
      .setUuid(getUUID)
      .addSection{
      Section.newBuilder
        .setUuid(getUUID)
        .setTextSpan(0 to doc.stringLength)
        .addSentenceSegmentation(doc.sentences)
        .build
    }
  }
    .build

  implicit def Document2MyDocument(doc:Document) = new MyDocument(doc)

  class MyDocument(doc:Document) {
    def asCommunication:Communication = Document2Communication(doc)
  }

  implicit def Communication2MyCommunication(comm:Communication) = new MyCommunication(comm)
    
  class MyCommunication(comm:Communication) {
    private val docWrapper = new DocumentWrapper(comm)
  	def asDocument:Seq[Document] = asDocument[StringVariable](new StringVariable("DEFAULT"))
    def asDocument[T<:StringVariable](annoTheory:T):Seq[Document] = {
  		if(annoTheory.value eq "DEFAULT") return Seq(new DocumentWrapper(comm).head)
  		else{
  			val classType = annoTheory.getClass()
  			return docWrapper.filter(doc => doc.attr.apply(classType).value==annoTheory.value).toSeq
  		} 
  	}
    def enumerateTheories // todo implement me to return a structure of annotation theories present in the communication
  }
}