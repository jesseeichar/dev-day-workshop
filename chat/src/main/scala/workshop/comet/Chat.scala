package workshop.comet

import scala.actors.Actor
import Actor._
import net.liftweb._
import http._
import js._
import SHtml._
import JsCmds._

import _root_.net.liftweb.widgets.gravatar.Gravatar
case class Messages(msgs: List[String])
object Clear

object ChatServer extends Actor with ListenerManager {
  private var msgs: List[String] = Nil

  protected def createUpdate = Messages(msgs)

  override def highPriority = {
    case Clear => 
        msgs = Nil
        updateListeners()
    case s: String if s.length > 0 =>
      msgs ::= s
      updateListeners()
  }

  this.start
}

class Chat extends CometActor with CometListenee
{
  private var msgs: List[String] = Nil
      
      def option(any:AnyRef) = if(any == null) None else Some(any)

      def username = theSession.initialHeaders.find( _._1 == "sec-username").map{_._2}.getOrElse("Anonymous")
      def email = theSession.initialHeaders.find( _._1 == "sec-email").map{_._2}
      def roles = {
          for{(name,value) <- theSession.initialHeaders
              if name == "sec-roles"
              role <- value.split(",") } yield role.toUpperCase
          List("ROLE_ADMIN")
      }
//  {if(email.isDefined) Gravatar(email.get)}
  def render =
<div>
    <p>Welcome to our little chat</p>
    <p>{ email.map{email => Gravatar(email)}.getOrElse("No gravatar")}</p>
    
    <ul>
      {
        msgs.reverse.map(m => <li>{m}</li>)
      }
      </ul>
      {
        ajaxText("", s => {ChatServer ! username+": "+s; Noop})
      }
      {
          if(roles.contains("ROLE_ADMIN")) {
              ajaxButton("Clear Log", () => {ChatServer ! Clear; Noop})
          } else {
              ""
          }
      }
</div>

  protected def registerWith = ChatServer

  override def highPriority = {
      case Messages(m) =>
        msgs = m ;
        reRender(false)
  }
}