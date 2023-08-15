package module1.futures

import HomeworksUtils.TaskSyntax

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence2[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
    task"Реализуйте метод `fullSequence`" ()


  def fullSequence[A](futures: List[Future[A]]): Future[(List[A], List[Throwable])] = {
    val futureList: Future[List[Try[A]]] = Future.sequence(futures.map(_.transform(Success(_))))
    val successes: Future[List[A]] = futureList
      .map(v => v.collect {
        case Success(x) => x
      })
    val failures: Future[List[Throwable]] = futureList
      .map(v => v.collect {
        case Failure(x)=>x
      })

    val res: Future[(List[A], List[Throwable])] = for {
      successFuture <- successes
      failedFuture <- failures
    } yield (successFuture, failedFuture)

    res
  }
}
