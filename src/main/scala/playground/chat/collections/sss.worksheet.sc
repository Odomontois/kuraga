trait Foo[E]{
	def get(): E
	def put(e: E): Unit
}


def bar(foo: Foo[_]): Unit = {
	val e = foo.get()
	foo.put(e)
}