package cn.okay.irt.context

import java.util.concurrent.{ExecutorService, Executors, ThreadPoolExecutor, TimeUnit}

/**
  * Created by zhangyikuo on 2017/4/20.
  */
object LocalRunner {
  // 本地线程池
  private var threadPool: ThreadPoolExecutor = _

  private def startPoolChecker() {
    val checker = new Thread(new Runnable {
      override def run() {
        try {
          var t = 0
          while (t < 2) {
            Thread.sleep(5 * TaskBuilder.learnInterval)

            this.synchronized {
              val needClose = !threadPool.isShutdown && !threadPool.isTerminated && threadPool.getActiveCount <= 0
              t = if (needClose) t + 1 else 0
            }
            if (t >= 2) {
              threadPool.shutdown()
              threadPool.awaitTermination(TaskBuilder.learnInterval, TimeUnit.MILLISECONDS)
              threadPool = null
            }
          }
        } catch {
          case e: Exception =>
            threadPool.shutdown()
            threadPool.awaitTermination(TaskBuilder.learnInterval, TimeUnit.MILLISECONDS)
            threadPool = null
        }
      }
    })
    checker.start()
  }

  def getExecutor: ExecutorService = {
    this.synchronized {
      if (null == threadPool || threadPool.isShutdown || threadPool.isTerminated) {
        threadPool = Executors.newFixedThreadPool(TaskBuilder.getLearnerThreadPoolSize).asInstanceOf[ThreadPoolExecutor]
        startPoolChecker()
      }
      threadPool
    }
  }
}
