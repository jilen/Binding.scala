import com.thoughtworks.binding.Binding.{ Var, Vars }
import com.thoughtworks.binding.Binding
import com.thoughtworks.binding._
import org.scalajs.dom.raw.Event
import org.scalajs.dom._
import scalatags.JsDom.all._

object Foo {

  object state {
    val data = Var[Seq[String]](Seq.empty)
    val seq = Var[Int](0)

  }

  @scalatag
  def render = {
    span(state.seq.bind.toString)
  }


  dom.render(document.body, render)
}
