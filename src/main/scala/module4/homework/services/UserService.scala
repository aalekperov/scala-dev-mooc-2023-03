package module4.homework.services

import module4.homework.dao.entity.UserId
import zio.Has
import zio.Task
import module4.homework.dao.entity.User
import module4.homework.dao.entity.Role
import module4.homework.dao.repository.UserRepository
import zio.ZIO
import zio.RIO
import module4.homework.dao.entity.UserToRole
import zio.ZLayer
import zio.macros.accessible
import module4.homework.dao.entity.RoleCode
import module4.phoneBook.db
import module4.phoneBook.db.DataSource

@accessible
object UserService{
    type UserService = Has[Service]

    trait Service{
        def listUsers(): RIO[db.DataSource, List[User]]
        def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]
        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]
        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
    }

    class Impl(userRepo: UserRepository.Service) extends Service{
        val dc = db.Ctx
        import dc._

        def listUsers(): RIO[db.DataSource, List[User]] =
            userRepo.list()

        def listUsersDto(users: List[User]):  RIO[db.DataSource,List[UserDTO]] = {
            ZIO.foreach(users) { user =>
                userRepo.userRoles(user.typedId).map( roles => UserDTO(user, roles.toSet))
            }
        }

        def listUsersDTO(): RIO[db.DataSource,List[UserDTO]] = {
            for {
                users <- listUsers()
                usersDto <- listUsersDto(users)
            } yield usersDto
        }

        def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = {

            val res: RIO[DataSource, List[UserDTO]] = for {
                user <- userRepo.createUser(user)
                _ <- userRepo.insertRoleToUser(roleCode, user.typedId)
                usersDto <- listUsersDto(List(user))
            } yield usersDto

            res.map(_.head)
        }

        def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource,List[UserDTO]] = {
            for {
                users <- userRepo.listUsersWithRole(roleCode)
                usersDto <- listUsersDto(users)
            } yield usersDto
        }
        
        
    }

    val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] = ZLayer.fromService(repo => new Impl(repo))
}

case class UserDTO(user: User, roles: Set[Role])