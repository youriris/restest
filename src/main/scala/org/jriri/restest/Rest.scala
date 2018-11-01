package org.jiris.restest

import org.apache.http.client.methods._
import org.apache.http.entity.StringEntity
import org.apache.http.Header
import org.apache.http.HttpResponse
import org.apache.http.impl.client.HttpClients
import org.apache.http.impl.client.DefaultHttpRequestRetryHandler
import org.apache.http.util.EntityUtils
import com.fasterxml.jackson.databind.node._
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.module._
import com.fasterxml.jackson.databind.introspect._
import com.fasterxml.jackson.databind.cfg.MapperConfig
import com.fasterxml.jackson.core.Version
import com.fasterxml.jackson.annotation._

import collection.JavaConverters._
import collection.mutable.Buffer
import concurrent.duration._
import io.Source
import language.dynamics
import reflect.runtime.universe._
import reflect.ClassTag
import scala.util.control.NonFatal

import java.io.InterruptedIOException
import java.net.ConnectException
import java.net.URI
import java.nio.charset.StandardCharsets
import java.util.Arrays
import javax.net.ssl.SSLException

trait JsImplicits {
  lazy val snakeNamingStrategy = new PropertyNamingStrategy.PropertyNamingStrategyBase {
    override def nameForField(config: MapperConfig[_], field: AnnotatedField, defaultName: String): String =
          toSnakeName(defaultName)

    override def nameForGetterMethod(config: MapperConfig[_], field: AnnotatedMethod, defaultName: String): String =
          toSnakeName(defaultName)

    def translate(name: String): String = name

    protected def toSnakeName(name: String) = name.replaceAll("([a-z])([A-Z])", "$1_$2").toLowerCase
  }

  def buildObjectMapper = {
    val module = new SimpleModule("MyModule", new Version(1, 0, 0, null, null, null))

    val mapper = new ObjectMapper().registerModule(module).
        setSerializationInclusion(JsonInclude.Include.NON_NULL).
        configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, true)

    val vc = mapper.getSerializationConfig().getDefaultVisibilityChecker().
        withFieldVisibility(JsonAutoDetect.Visibility.ANY).asInstanceOf[VisibilityChecker[_]].
        withGetterVisibility(JsonAutoDetect.Visibility.NONE).asInstanceOf[VisibilityChecker[_]].
        withSetterVisibility(JsonAutoDetect.Visibility.NONE).asInstanceOf[VisibilityChecker[_]].
        withCreatorVisibility(JsonAutoDetect.Visibility.NONE).asInstanceOf[VisibilityChecker[_]]
    mapper.setVisibility(vc)
    mapper.enable(SerializationFeature.INDENT_OUTPUT)
    mapper.setPropertyNamingStrategy(snakeNamingStrategy)

    mapper
  }
  
  def camelCaseObjectMapper = {
    buildObjectMapper.setPropertyNamingStrategy(new PropertyNamingStrategy.PropertyNamingStrategyBase {
      override def nameForField(config: MapperConfig[_], field: AnnotatedField, defaultName: String): String = defaultName

      def translate(name: String): String = name
    })
  }
  
  private lazy val defaultObjectMapper = buildObjectMapper

  implicit class StringToJsonWrapper(val str: String) {
    def toJson = new ObjectMapper().readTree(str)
  }

  implicit class StringToDynNode(str: String) {
    def $ = DynNode(Some(str.toJson))
    def as[R: JsonReads: TypeTag]: Option[R] = str.$.as[R]
    def asArray[R: JsonReads: TypeTag] = str.$.asArray[R]
  }

  trait JsonReads[V] {
    def reads(src: JsonNode): V

    def objectMapper = buildObjectMapper
  }

  object JsonReads {
    lazy val mirror = runtimeMirror(getClass.getClassLoader)
    
    implicit def defaultJsonReads[V : TypeTag] = new JsonReads[V] {
      def reads(src: JsonNode): V = {
        val cls = mirror.runtimeClass(typeTag[V].tpe.typeSymbol.asClass)
        objectMapper.readValue(src.toString, cls).asInstanceOf[V]
      }
      
      override def toString = "SnakeCase JsonReads"
    }

    def camelCase[V : TypeTag] = new JsonReads[V] {
      def reads(src: JsonNode): V = {
        val cls = mirror.runtimeClass(typeTag[V].tpe.typeSymbol.asClass)
        objectMapper.readValue(src.toString, cls).asInstanceOf[V]
      }
      
      override def toString = "CamelCase JsonReads"
      
      override def objectMapper = camelCaseObjectMapper
    }
  }

  case class DynNode(n: Option[JsonNode]) extends Dynamic {
    
    /** Convert the JSON node to a double, converting if needed.
     *  
     *  Note: this can silently convert a JSON string to double.
     *  For tests, use doubleValue instead.
     */
    def asDouble = if(n.isDefined) Some(n.get.asDouble) else None
    
    /** Convert the JSON node to an int, converting if needed.
     *  
     *  Note: this can silently convert a JSON string to int.
     *  For tests, use intValue instead.
     */
    def asInt = if(n.isDefined) Some(n.get.asInt) else None
    
    /** Convert the JSON node to a String, converting if needed.
     *  
     *  Note: this can silently convert a JSON number to a String.
     *  For tests, use textValue instead.
     */
    def asText = if(n.isDefined) Some(n.get.asText) else None
    
    /** Convert the JSON node to a boolean, converting if needed.
     *  
     *  Note: this can silently convert other JSON types to a boolean.
     *  For tests, use booleanValue instead.
     */
    def asBoolean = if(n.isDefined) Some(n.get.asBoolean) else None
    
    /** Convert the JSON node to a double ONLY if it is a JSON number. */
    def doubleValue = if(n.isDefined && n.get.isNumber) Some(n.get.doubleValue) else None
    
    /** Convert the JSON node to an int ONLY if it is an integral JSON number that fits in an int. */
    def intValue = if(n.isDefined && n.get.isIntegralNumber && n.get.canConvertToInt) Some(n.get.intValue) else None
    
    /** Convert the JSON node to an int ONLY if it is an integral JSON number that fits in a long. */
    def longValue = if(n.isDefined && n.get.isIntegralNumber && n.get.canConvertToLong) Some(n.get.longValue) else None
    
    /** Convert the JSON node to a String ONLY if it is a JSON string. */
    def textValue = if(n.isDefined && n.get.isTextual) Some(n.get.textValue) else None
    
    /** Convert the JSON node to a boolean ONLY if it is a JSON boolean. */
    def booleanValue = if(n.isDefined && n.get.isBoolean) Some(n.get.booleanValue) else None
    
    /** Get the node as Option[JsonNode]. */
    def asJsonNode = n

    def as[R: JsonReads: TypeTag]: Option[R] = if(n.isDefined) Some(implicitly[JsonReads[R]].reads(n.get)) else None

    def asArray[R: JsonReads: TypeTag]: Option[Array[R]] = {
      implicit val clsTag = ClassTag[R]( typeTag[R].mirror.runtimeClass( typeTag[R].tpe ) )
      if(n.isDefined) Option(n.get.iterator.asScala.map(implicitly[JsonReads[R]].reads(_)).toArray) else None
    }

    def apply(name: String) = DynNode(if(n.isDefined) Option(n.get.get(name)) else None)

    def apply(index: Int) = DynNode(if(n.isDefined) Option(n.get.get(index)) else None)

    def selectDynamic(name: String) = apply(name)

    def applyDynamic(name: String)(index: Int) = apply(name).apply(index)

    override def toString = {
      n match {
        case Some(n) =>
          n match {
            case v: TextNode => v.asText
            case _ => n.toString
          }
        case _ => "null"
      }
    }
    
    /**
     * Makes this Traversable, so "for" comprehensions can be used.
     */
    def foreach[U] (f: DynNode => U) = {
      if (n.isDefined) {
        var idx = 0
        while (idx < n.get.size) {
          f(apply(idx))
          idx += 1
        }
      }
    }
  }
}

trait Rest extends JsImplicits {
  def $code = lastResponse.get.code
  def $body = lastResponse.get.body
  def $headers = lastResponse.get.headers
  def $header(name: String) = $headers.find(_.getName == name) match {
    case Some(header) => header.getValue
    case _ => null
  }
  def $response = $body.$

  val testHttpClient = HttpClients.custom.setRetryHandler(new DefaultHttpRequestRetryHandler(
    3, false,
    Arrays.asList(classOf[InterruptedIOException], classOf[ConnectException], classOf[SSLException])) {
      java.security.Security.setProperty("networkaddress.cache.negative.ttl", "0");
  }).disableRedirectHandling.build

  private val lastResponse = new ThreadLocal[CallResponse]

  def retry[R](times: Int, interval: Duration, exceptions: Class[_ <: Throwable]*)(block: => R) = {
    var result: Option[R] = None

    (1 to times).takeWhile { i =>
      try {
        result = Some(block)
        false
      } catch {
        case NonFatal(e) if exceptions.exists(_.isInstance(e)) || exceptions.isEmpty =>
          if (i == times) throw e
          else {
            Thread.sleep(interval.toMillis)
            true
          }
      }
    }

    result.get
  }

  implicit class StatementWrapper(val str: String) {
    val location = new Exception().getStackTrace.dropWhile { e => 
        e.getClassName.startsWith("com.acti.commons") ||
        e.getClassName.startsWith("it.restapi") ||
        e.getClassName.startsWith("java") ||
        e.getClassName.startsWith("scala")
    }.headOption match {
      case Some(e) =>
        val r = "(.*?)\\$.*".r
        val root = e.getClassName match {
          case r(root) => root 
          case root => root 
        }
        s"($root:${e.getLineNumber})"
      case _ => ""
    }
    
    def state = println(s"\n  ============= $str$location =============")
  }

  implicit class HttpInterpolator(val sc: StringContext) extends Enumeration {
    protected val header = "(.*?)\\:(.*)".r
    protected val stater = "\\#\\s*(.*)".r

    def get(args: Any*)(implicit writes: HttpWrites, trace: Trace) = execute(args: _*)(new HttpGet(), false)

    def head(args: Any*)(implicit writes: HttpWrites, trace: Trace) = execute(args: _*)(new HttpHead(), false)

    def post(args: Any*)(implicit writes: HttpWrites, trace: Trace) = execute(args: _*)(new HttpPost(), true)

    def put(args: Any*)(implicit writes: HttpWrites, trace: Trace) = execute(args: _*)(new HttpPut(), true)

    def delete(args: Any*)(implicit writes: HttpWrites, trace: Trace) = execute(args: _*)(new HttpDelete(), false)
    
    def options(args: Any*)(implicit writes: HttpWrites, trace: Trace) = execute(args: _*)(new HttpOptions(), false)

    protected def execute(args: Any*)(request: HttpRequestBase, bodyAllowed: Boolean)
        (implicit writes: HttpWrites, trace: Trace): CallResponse = {
      val body = parse(sc.standardInterpolator(str => StringContext.treatEscapes(str), args), request, bodyAllowed)
      trace.request(request, writes.write(request, body))
      transact(request)
    }
    
    type State = Val
    val s0, s1, s2, s3, s4, s5 = Value

    /*
    digraph http_state_machine {
      rankdir=LR;
      size="8,5"
      node [shape = circle]
      s0 -> s0 [label = "empty"]
      s0 -> s1 [label = "#/comments"]
      s0 -> s2 [label = "non-empty/uri"]
      s1 -> s1 [label = "empty | #"]
      s1 -> s2 [label = "non-empty/uri"]
      s2 -> s2 [label = "non-empty/header"]
      s2 -> s3 [label = "empty"]
      s3 -> s4 [label = "non-empty/body"]
      s3 -> s3 [label = "empty"]
      s4 -> s4 [label = "non-empty/body"]
      s4 -> s5 [label = "empty/done"]
      s5 -> s5 [label = "empty"]
      s5 -> s4 [label = "1. non-empty/body"]
      s5 -> error [label = "2. non-empty"]
    }
    */
    protected def parse(content: String, request: HttpRequestBase, bodyAllowed: Boolean)(implicit writes: HttpWrites) = {
      val body = Buffer[String]()
      var state = s0

      val lines = Source.fromString(content).getLines().zipWithIndex.toList
      lines.foreach { i =>
        val (line, index) = (i._1, i._2 + 1)
        val dump = () => s"at line #$index!!\n" + lines.map(line => "%3s".format(line._2 + 1) + ": " + line._1).mkString("\n")

        state match {
          case `s0` => line.trim match {
            case "" => // ignore
            case stater(s) => s.state; state = s1 // if line starts with #, print out the rest
            case s => "invoking".state; request.setURI(new URI(s)); state = s2
          }
          case `s1` => line.trim match {
            case "" => // ignore
            case stater(s) => // ignore
            case s => request.setURI(new URI(s)); state = s2
          }
          case `s2` => line.trim match {
            case "" => state = s3
            case header(n, v) => request.addHeader(n, v)
            case s => throw new RuntimeException(s"Value cannot be used as an HTTP header an http header ${dump()}")
          }
          case `s3` => line.trim match {
            case "" => // ignore
            case s =>
              if(!bodyAllowed) throw new RuntimeException(s"HTTP ${request.getMethod} cannot have body ${dump()}")
              else body += line; state = s4
            }
          case `s4` => line.trim match {
            case "" => state = s5
            case s => body += line
          }
          case `s5` => line.trim match {
            case "" => // ignore
            case s =>
              if(writes.allowEmptyLinesInBody) {
                body += line
                state = s4
              } else throw new RuntimeException(s"HTTP body has been already completed ${dump()}")
          }
        }
      }

      body
    }

    protected def transact(request: HttpRequestBase)(implicit trace: Trace) = {
      val response = retry(10, 1 seconds) {
        testHttpClient.execute(request)
      }
      trace.response(response)
      val r =
        try {
          if (response.getEntity != null) {
            val entity = EntityUtils.toString(response.getEntity, StandardCharsets.UTF_8)
            EntityUtils.consume(response.getEntity)
            trace.responseBody(entity)
            CallResponse(response.getStatusLine.getStatusCode, response.getAllHeaders, entity, response)
          } else {
            CallResponse(response.getStatusLine.getStatusCode, response.getAllHeaders, null, response)
          }
        } finally {
          response.close()
        }
      lastResponse.set(r)
      r
    }
  }
  
  object JsonTrimmer {
    def apply(body: Buffer[String]) = {
      val indentation = body.foldLeft(Integer.MAX_VALUE){ (min, line) =>
        if(line.trim == "") min 
        else Integer.min(line.takeWhile(Character.isSpace(_)).size, min)
      }
      body.map{ line => if(line.length > indentation) line.substring(indentation) else line}
    }
  }

  trait HttpWrites {
    def write(request: HttpRequestBase, body: Buffer[String]): Buffer[String]

    def allowEmptyLinesInBody: Boolean
  }

  object HttpWrites {
    implicit val defaultWrites: HttpWrites = new JsonHttpWrites
  }

  class JsonHttpWrites extends HttpWrites {
    def write(request: HttpRequestBase, body: Buffer[String]): Buffer[String] = {
      request match {
        case r: HttpPost =>
          adjustContentTypeHeader(request)
          val json = JsonTrimmer(body)
          r.setEntity(new StringEntity(json.mkString("\n"), "UTF-8"))
          json
        case r: HttpPut =>
          adjustContentTypeHeader(request)
          val json = JsonTrimmer(body)
          r.setEntity(new StringEntity(json.mkString("\n"), "UTF-8"))
          json
        case _ => Buffer[String]()
      }
    }

    protected def adjustContentTypeHeader(request: HttpRequestBase) {
      request.getHeaders("Content-Type") match {
        case a if a.isEmpty => request.addHeader("Content-Type", "application/json")
        case a if a.size > 1 => throw new RuntimeException(s"Multiple Content-Type headers(${a.size}) are not allowed!")
        case a if a(0).getValue.trim == "application/json" => // good
        case a if a(0).getValue.trim == "application/octet-stream" => // good
        case a if a(0).getValue.trim == "application/x-www-form-urlencoded" => // good
        case a if a(0).getValue.trim == "text/csv" => // good
        case a => throw new RuntimeException(s"Conflicting content-type found(${a.toList})!")
      }
    }

    def allowEmptyLinesInBody = true
  }

  protected case class CallResponse(code: Int, headers: Array[Header], body: String, response: HttpResponse)

  trait Trace {
    def request(request: HttpRequestBase, body: Buffer[String])

    def response(response: HttpResponse)

    def responseBody(body: String)
  }

  object Trace {
    implicit def defaultTrace = new SystemOutTrace
  }

  class SystemOutTrace extends Trace {
    def request(request: HttpRequestBase, body: Buffer[String]) {
      out(s"${request.getMethod} ${request.getURI}")
      request.getAllHeaders.foreach{ header =>
        out(s"${header.getName} : ${header.getValue}")
      }

      if(!body.isEmpty) {
        out()
        Source.fromString(body.mkString("\n").trim.stripLineEnd).getLines.foreach { line =>
          out(s"$line")
        }
      }
    }

    def response(response: HttpResponse) {
      neutral
      in(s"${response.getStatusLine}")
      response.getAllHeaders.foreach{ header =>
        in(s"${header.getName} : ${header.getValue}")
      }
    }

    def responseBody(body: String) {
      if( body != null && !body.trim.isEmpty) {
        in()
        Source.fromString(body.trim.stripLineEnd).getLines.foreach { line =>
          in(s"$line")
        }
      }
    }
    
    protected def out(line: String = "") = println(s"  >> $line")
    protected def in(line: String = "") = println(s"  << $line")
    protected def neutral = println(s"")
  }
}