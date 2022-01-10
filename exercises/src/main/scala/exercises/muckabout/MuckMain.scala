package exercises.muckabout

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}

object MuckMain extends App {

  val fixedPool                                  = Executors.newFixedThreadPool(2)
  val executionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(fixedPool)

  def createRunnable(n: Int): Runnable =
    () => {
      val currentThread = Thread.currentThread();
      println(s"Runnable $n on ${currentThread.getName}")
    }

  def createThread(n: Int): Thread = new Thread {
    override def run(): Unit = {
      val currentThread = Thread.currentThread()
      println(s"Thread run: $n on ${currentThread.getName}")
      super.run()
    }
  }

  1.to(10).map(createThread).foreach(_.start())

  (1 to 4).map(createRunnable).foreach(fixedPool.submit)

  val future = Future {
    val currentThread = Thread.currentThread();
    Thread.sleep(1000);
    println(s"Fut 1 on ${currentThread.getName}"); 1
  }(executionContext)
  val future2 = Future {
    val currentThread = Thread.currentThread();
    Thread.sleep(2000);
    println(s"Fut 2 on ${currentThread.getName}"); 2
  }(executionContext)
  val r1 = Await.result(future, 2.minutes)
  val r2 = Await.result(future2, 2.minutes)
  println(s"Res $r1 and $r2")
}
