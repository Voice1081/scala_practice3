package exercises

object Exercise {

  // 0. Вам нужно реализовать ADT дерева. Трейт для декомпозиции: Derevo. Наследники: Listok и Vetka.

  // 1 Вам нужно реализовать вспомогательный конструктор для тестов.
  // 2. Напишите функцию вычисления размера, которая подсчитывает количество узлов (листьев и ветвей) в дереве.
  // 3. Напишите функцию вычисления максимума, которая возвращает максимальный элемент в дереве(тип [Int]).
  // 4. Напишите функцию вычисления глубины, которая возвращает максимальную длину пути от корня дерева до любого листа.
  // 5. Напишите функцию map, которая изменяет сопоставляет каждый элемент дерева с заданной функцией.

  // 6. Напишите функцию вычисления размера(sizeF), которая подсчитывает количество узлов (листьев и ветвей)
  // в дереве через fold.
  // 7. Напишите функцию вычисления максимума(maximumF), которая возвращает максимальный элемент в дереве [Int]. (Заметка:
  //В Scala вы можете использовать x.max (y) или x max y, чтобы вычислить максимум двух целых чисел x
  //и у.) через fold
  // 8. Напишите функцию вычисления глубины (depthF), которая возвращает максимальную длину пути от корня
  // дерева до любого листа через fold
  // 9. Напишите функцию mapF, которая изменяет сопоставляет каждый элемент дерева с заданной функцией через fold.

  //Подсказка
  //Вы можете использовать x.max (y) или x max y, чтобы вычислить максимум двух целых чисел x и у.

  trait Derevo[A]
  case class Vetka[A](lft: Derevo[A], rgt: Derevo[A]) extends Derevo[A]
  case class Listok[A](value: A) extends Derevo[A]

  object Derevo {
    def vetka[A](lft: Derevo[A], rgt: Derevo[A]): Derevo[A] = Vetka(lft, rgt)
    def listok[A](a: A): Derevo[A] = Listok(a)
  }

  def size[A](t: Derevo[A]): Int = {
    t match {
      case Listok(value) => 1
      case Vetka(lft, rgt) => size(lft) + size(rgt) + 1
    }
  }

  def maximum(t: Derevo[Int]): Int = {
    t match {
      case Listok(value) => value
      case Vetka(lft, rgt) => maximum(lft) max maximum(rgt)
    }
  }

  def map[A, B](t: Derevo[A])(f: A => B): Derevo[B] = {
    t match {
      case Listok(value) => Listok(f(value))
      case Vetka(lft, rgt) => Vetka(map(lft)(f), map(rgt)(f))
    }
  }

  def fold[A, B](t: Derevo[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Listok(value) => f(value)
      case Vetka(lft, rgt) => g(fold(lft)(f)(g), fold(rgt)(f)(g))
    }
  }

  def depth[A](t: Derevo[A]): Int = {
    t match {
      case Listok(value) => 0
      case Vetka(lft, rgt) => depth(lft) + 1 max depth(rgt) + 1
    }
  }


  def depthF[A](t: Derevo[A]): Int = {
    fold(t)((a: A) => 0)((i: Int, j: Int) => i + 1 max j + 1)
  }

  def sizeF[A](t: Derevo[A]): Int = {
    fold(t)((a: A) => 1)((i: Int, j: Int) => i + j + 1)
  }

  def maximumF(t: Derevo[Int]): Int = {
    fold(t)((a: Int) => a)((i: Int, j: Int) => i max j)
  }

  def mapF[A, B](t: Derevo[A])(f: A => B): Derevo[B] = {
    fold(t)((a: A) => Listok(f(a)).asInstanceOf[Derevo[B]])((lft: Derevo[B], rgt: Derevo[B]) => Vetka(lft, rgt).asInstanceOf[Derevo[B]])
  }

}