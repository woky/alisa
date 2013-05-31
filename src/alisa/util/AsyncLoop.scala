package alisa.util

class AsyncLoop(sleepTime: Long, proc: () => Unit) {

	private var shutdown = false

	private val monitor = new Object

	private val t = new Thread {

		start

		override def run {
			while (!shutdown) {
				monitor.synchronized {
					try {
						monitor.wait(10 * 60 * 1000)
					} catch {
						case _: InterruptedException => shutdown = true
					}

					proc()
				}
			}
		}
	}

	def stop {
		shutdown = true
		monitor.synchronized {
			monitor.notifyAll
		}
		t.join
	}
}
