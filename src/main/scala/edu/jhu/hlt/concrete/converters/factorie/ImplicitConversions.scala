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
import cc.factorie.app.nlp.lemma.TokenLemma

/**
 * @author John Sullivan, tanx
 * This object contains implicit conversions between Concrete Communications
 * and FactorIE Documents.
 */
object ImplicitConversions {

  /*
    Methods to convert Documents to Communications

   */

  def getUUID:ConUUID = {
    val uuid = UUID.randomUUID
    ConUUID.newBuilder.setHigh(uuid.getMostSignificantBits).setLow(uuid.getLeastSignificantBits).build
  }

  implicit val factorieMetadata:AnnotationMetadata = AnnotationMetadata.newBuilder
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
    .setTextSpan(0 to facTok.string.length)
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

  implicit def Tags2Tagging(toks:Iterable[Option[TaggedToken]])(implicit meta:AnnotationMetadata):TokenTagging = TokenTagging.newBuilder
    .setUuid(getUUID)
    .setMetadata(meta)
    .addAllTaggedToken(toks.flatten.asJava)
    .build

  implicit def TokenIterable2Tokenization(tokens:Iterable[Token])(implicit uuid:ConUUID, meta:AnnotationMetadata):Tokenization = {
    Tokenization.newBuilder
      .setUuid(uuid)
      .setMetadata(meta)
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

  private def sentenceToDependencyParse(sent:Sentence, uuid:ConUUID)(implicit meta:AnnotationMetadata):DependencyParse = DependencyParse.newBuilder
    .setUuid(getUUID)
    .setMetadata(meta)
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
      .setTextSpan(0 to facSent.toString.length)
      .addTokenization(facSent.toSeq)
      .addDependencyParse(sentenceToDependencyParse(facSent, tokUUID))
      .build
  }

  implicit def FacConSentenceSegmentation(sents:Iterable[Sentence])(implicit meta:AnnotationMetadata):SentenceSegmentation = SentenceSegmentation.newBuilder
    .setUuid(getUUID)
    .setMetadata(meta)
    .addAllSentence((sents map Fac2ConSentence).asJava)
    .build

  private def Document2Communication(doc:Document)(implicit meta:AnnotationMetadata):Communication = Communication.newBuilder
    .setUuid(getUUID)
    .setGuid(doc.name)
    .setText(doc.string)
    .addSectionSegmentation{
    SectionSegmentation.newBuilder // Factorie doesn't split documents into paragraphs
      .setUuid(getUUID)
      .setMetadata(meta)
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
  
  /*
    Methods to convert Communications to Documents

   */ 
  private def ConFacTokenTagging(docWrapper:DocumentWrapper, tokTaggings: Iterable[TokenTagging], tagClass:String):Unit={
  	tokTaggings.foreach(tokTagging=>{
  		val theory = tokTagging.getMetadata().getTool()
  		val documents = tagClass match{
  			case "PosTag" => docWrapper.setTheory[PosTagTheory](new PosTagTheory(theory))
  			case "NerTag" => docWrapper.setTheory[NerTagTheory](new NerTagTheory(theory))
  			case "Lemma" => docWrapper.setTheory[LemmasTheory](new LemmasTheory(theory))
  		}
  		documents.foreach(document=>{
  			val sentence = document.sentences.last
  			tokTagging.getTaggedTokenList.asScala.foreach(tagTok=>{
  				val tokId = tagTok.getTokenId
  				val tokTag = tagTok.getTag()
  				sentence.tokens.filter(token=>token.attr[TokenId].value==tokId)
  					.foreach(token=>{tagClass match{
  							case "PosTag" => token.attr+=new MyPosLabel(token, tokTag)
  							case "NerTag" => token.attr+=new MyNerLabel(token, tokTag)
  							case "Lemma" => token.attr+=new TokenLemma(token, tokTag)
  						}
  					})
  			})
  		})
  	})
  }
    
  private def ConFacTokenization(docWrapper:DocumentWrapper, tokenizations: Iterable[Tokenization]):Unit={
  	tokenizations.foreach(tokenization=>{
  		docWrapper.setTheory[TokenizationTheory](new TokenizationTheory(tokenization.getMetadata().getTool())).foreach(document=>{
  			val sentence = document.sentences.last
  			tokenization.getTokenList.asScala.foreach(tok=>{
  				val token = new Token(sentence, tok.getText())
  				document.appendString(" ")
  				token.attr+=new TokenId(tok.getTokenId)
  			})
  			ConFacTokenTagging(docWrapper, tokenization.getPosTagsList.asScala, "PosTag")
  			ConFacTokenTagging(docWrapper, tokenization.getNerTagsList.asScala, "NerTag")
  			ConFacTokenTagging(docWrapper, tokenization.getLemmasList.asScala, "Lemma")
  			// DEPENDENCE PARSE see cc.factorie.app.nlp.parse.ParseLabel.scala
  		})
  	})
  }
    
  private def ConFacSentenceSegmentation(docWrapper:DocumentWrapper, sentSegs:Iterable[SentenceSegmentation]):Unit={
  	sentSegs.foreach(sentSeg=>{
  		docWrapper.setTheory[SentenceSegmentationTheory](new SentenceSegmentationTheory(sentSeg.getMetadata().getTool())).foreach(document=>{
  			sentSeg.getSentenceList.asScala.foreach(sent=>{
  				new Sentence(document)
  				ConFacTokenization(docWrapper, sent.getTokenizationList.asScala)
  			})
  		})
  	})
  }
    
  private def ConFacSectionSegmentation(docWrapper:DocumentWrapper, sss:Iterable[SectionSegmentation]):Unit={
  	sss.foreach(ss=>{
  		docWrapper.setTheory[SectionSegmentationTheory](new SectionSegmentationTheory(ss.getMetadata().getTool()))
  		ss.getSectionList.asScala.foreach(sec=>ConFacSentenceSegmentation(docWrapper, sec.getSentenceSegmentationList.asScala))
  	})
  }
  
  private def Communication2DocumentWrapper(comm:Communication): DocumentWrapper ={
  	val docWrapper = new DocumentWrapper(comm)
  	ConFacSectionSegmentation(docWrapper, comm.getSectionSegmentationList.asScala)
  	return docWrapper
  } 

  implicit def Communication2MyCommunication(comm:Communication) = new MyCommunication(comm)
    
  class MyCommunication(comm:Communication) {
  	def asDocument:Document = asDocument[StringVariable](new StringVariable("DEFAULT")).head
    def asDocument[T<:StringVariable](annoTheory:T):Seq[Document] = {
    	val docWrapper = Communication2DocumentWrapper(comm)
    	println("Got documentWrapper: "+docWrapper.size+" level")
  		if(annoTheory.value eq "DEFAULT") return docWrapper.iterator.toSeq//Seq(new DocumentWrapper(comm).head)
  		else{
  			val classType = annoTheory.getClass()
  			return docWrapper.filter(doc => doc.attr.apply(classType).value==annoTheory.value).toSeq
  		} 
  	}
    def enumerateTheories = null// todo implement me to return a structure of annotation theories present in the communication
  }
}