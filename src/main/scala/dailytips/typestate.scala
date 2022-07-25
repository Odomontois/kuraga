package dailytips.typestate

type UserId
type DirectoryId
type User
type Directory
type Response

enum Actions[+Result]:
  case Noop
  def flatMap[Next](next: Result => Actions[Next]): Actions[Next] = Noop

trait RespondWith[A]:
  extension (data: A) def respond: Actions[Response]

given RespondWith[Unit]      = _ => Actions.Noop
given RespondWith[User]      = _ => Actions.Noop
given RespondWith[Directory] = _ => Actions.Noop

trait ReadAccess
trait WriteAccess

import Command.*

trait UserService:
  extension (userId: UserId)
    def readUser(using ReadAccess): Actions[User]
    def writeUser(user: User)(using WriteAccess): Actions[Unit]

trait DirectoryService:
  extension (id: DirectoryId)
    def readDirectory(using ReadAccess): Actions[Directory]
    def changeDirectory(data: Directory)(using WriteAccess): Actions[Unit]

enum Command:
  case GetUser(id: UserId, read: ReadAccess)
  case UpdateUser(id: UserId, data: User, write: WriteAccess)
  case GetDir(id: DirectoryId, read: ReadAccess)
  case UpdateDir(id: DirectoryId, data: Directory, write: WriteAccess)

def handle(command: Command)(using DirectoryService, UserService): Actions[Response] =
  command match
    case GetUser(id, given ReadAccess)           => id.readUser.flatMap(_.respond)
    case UpdateUser(id, data, given WriteAccess) => id.writeUser(data).flatMap(_.respond)
    case GetDir(id, given ReadAccess)            => id.readDirectory.flatMap(_.respond)
    case UpdateDir(id, data, given WriteAccess)  => id.changeDirectory(data).flatMap(_.respond)
