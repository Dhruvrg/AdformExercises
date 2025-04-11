package exercises

import lectures.implicits.TypeClasses.{User, user}

object EqualityPlayground extends App {

  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  implicit object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  object FullEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean =
      (a.name == b.name && a.email == b.email)
  }

  // Implement the TC pattern for the Equality tc

  object Equal {
    def apply[T](a: T, b: T)(implicit equalizer: Equal[T]): Boolean =
      equalizer.apply(a, b)
  }

  // This is called AD-HOC polymorphism
  val dhruv = User("Dhruv", 22, "dhruvrg2003@gmail.com")
  val anotherDhruv = User("Dhruv", 22, "anotherUser@gmail.com")
  println(Equal(dhruv, anotherDhruv))


  implicit class TypeSafeEqual[T](value: T) {
    def ===(other: T)(implicit equalizer: Equal[T]): Boolean = equalizer.apply(value, other)
    def !==(other: T)(implicit equalizer: Equal[T]): Boolean = ! equalizer.apply(value, other)
  }
  println(dhruv === anotherDhruv)
  // it is rewritten as new TypeSafeEqual[User](dhruv).===(anotherDhruv)(NameEquality)
  println(dhruv !== anotherDhruv)
}
