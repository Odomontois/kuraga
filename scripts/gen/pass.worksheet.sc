import java.security.SecureRandom
import scala.collection.immutable.WrappedString
import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
val sr = SecureRandom.getInstanceStrong()

val alpha = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')).mkString

val p = WrappedString.fill(42)(alpha(sr.nextInt(alpha.length()))).mkString

val cp = Toolkit.getDefaultToolkit().getSystemClipboard()

cp.setContents(StringSelection(p), null)

1 + 2
