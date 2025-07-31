def having[T, R](e: T)(body: T ?=> R): R = body(using e)

class Config(val cfg: String)
class Context(val ctx: String)

def foo(value: String)(using cfg: Config)()(using ctx: Context) =
  s"value = ${value},cfg = ${cfg.cfg}, ctx = ${ctx.ctx}"

having(Context("ctx A")):
    having(Config("cfg A")):
        foo("A1")()(using Context("ctx A"))
  
