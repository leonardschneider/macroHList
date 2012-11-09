package macroHList

/** Thanks to Miles Sabin
 */

object TypeOperators {
  def unexpected : Nothing = sys.error("Unexpected invocation")

  /** Following additions by Leonard Schneider
   *  Type logical operators
   */

  trait &:&[A, B]

  implicit def andTypeOp[A, B](implicit e1: A, e2: B): A &:& B = new &:&[A, B] {}

  trait !:![A]

  implicit def notTypeOp[A]: !:![A] = new !:![A] {}
  implicit def notTypeOpAmbig1[A](implicit e: A): !:![A] = unexpected
  
  type |:|[A, B] = !:![!:![A] &:& !:![B]]

  // Basic definitions
  type Id[+T] = T
  type Const[C] = {
    type Kind[T] = C
  }
  type ¬[T] = T => Nothing
  type ¬¬[T] = ¬[¬[T]]
  type ∧[T, U] = T with U
  type ∨[T, U] = ¬[¬[T] ∧ ¬[U]]
  
  // Type-lambda for context bound
  type |∨|[T, U] = {
    type λ[X] = ¬¬[X] <:< (T ∨ U) 
  }

  // Type inequalities
  trait =:!=[A, B] 

  implicit def neq[A, B] : A =:!= B = new =:!=[A, B] {}
  implicit def neqAmbig1[A] : A =:!= A = unexpected
  implicit def neqAmbig2[A] : A =:!= A = unexpected
  
  trait <:!<[A, B]

  implicit def nsub[A, B] : A <:!< B = new <:!<[A, B] {}
  implicit def nsubAmbig1[A, B >: A] : A <:!< B = unexpected
  implicit def nsubAmbig2[A, B >: A] : A <:!< B = unexpected

  // Type-lambda for context bound
  type |¬|[T] = {
    type λ[U] = U <:!< T
  }


}
