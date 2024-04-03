package codegen

import collection.mutable
import ast._

class codegen {
  val skeleton = """#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
struct f {
  uint32_t v;
  struct f *n;
};
struct f *s;
struct t {
  struct f *t;
  struct f *b;
  bool a;
  struct t *n;
};
struct t *g = 0;
static void p(uint32_t x) { s->v += x; }
static void n(uint32_t x) {
  struct f *f = malloc(sizeof(struct f));
  f->v = x;
  f->n = s;
  s = f;
}
static void d(uint32_t x, bool a) {
  if (g->a) {
    g->b->v += x;
  } else {
    struct f *f = malloc(sizeof(struct f));
    f->v = x;
    f->n = 0;
    g->b->n = f;
    g->b = f;
  }
  g->a = a;
}
static void i() {
  struct t *t = malloc(sizeof(struct t));
  struct f *f = malloc(sizeof(struct f));
  f->v = 0;
  f->n = 0;
  t->t = f;
  t->b = f;
  t->a = true;
  t->n = g;
  g = t;
}
static void o() {
  if (g->a) {
    g->b->v += s->v;
    struct f *f = s;
    s = s->n;
    free(f);
  }
  g->b->n = s;
  s = g->t;
  struct t *t = g;
  g = g->n;
  free(t);
}
static bool m() {
  if (s->v > 0) {
    s->v--;
    return true;
  } else {
    if (s->n == 0) {
      return false;
    }
    struct f *f = s;
    s = s->n;
    free(f);
    return false;
  }
}
static void inspect() {
  struct f *f = s;
  while (f != 0) {
    printf("{%d}", f->v);
    f = f->n;
  }
  printf("\n");
  struct t *t = g;
  while (t != 0) {
    printf("  [");
    struct f *f = t->t;
    while (f != 0) {
      printf("{%d}", f->v);
      f = f->n;
    }
    if (t->a) {
      printf("+");
    }
    printf("]");
    printf("{%d}\n", t->b->v);
    t = t->n;
  }
}
static void r() {
  struct t *t = g->n;
  if (t->a) {
    struct f *o = g->t;
    t->b->v += o->v;
    if (o->n == 0) {
      o->v = 0;
      g->a = true;
    } else {
      g->t = o->n;
      free(o);
    }
  }
  t->a = g->a;
  t->b->n = g->t;
  t->b = g->b;
  struct t *f = g;
  g = t;
  free(f);
}
static void h() {
  while (s->v != 0) {
    printf("%c", (char)s->v);
    struct f *f = s;
    if (s->n == 0) return;
    s = s->n;
    free(f);
  }
  if (s->n == 0) return;
  struct f *f = s;
  s = s->n;
  free(f);
}
static void op() {
  if (s->n != 0) {
    struct f *f = s;
    s = s->n;
    free(f);
  }
}
static void os() {
  d(s->v, false);
  op();
}
static void oa() {
  if (s->n != 0) {
    s->n->v += s->v;
    op();
  }
}
static void om() {
  if (s->n != 0) {
    if (s->n->v < s->v) {
      s->n->v = 0;
    } else {
      s->n->v -= s->v;
    }
  }
  op();
}
static void oc() {
  n(s->v);
}
static void ox() {
  if (s->n != 0) {
    s->n->v *= s->v;
  }
  op();
}
static void od() {
  if (s->n == 0) {
    s->v = 0;
    return;
  }
  uint32_t n = s->n->v;
  uint32_t d = s->v;
  s->n->v = n / d;
  s->v = n % d;
}
static void ow() {
  if (s->n == 0) {
    n(0);
    return;
  }
  uint32_t v = s->n->v;
  s->n->v = s->v;
  s->v = v;
}"""
  val mainStart = """int main() {
  s = malloc(sizeof(struct f));
  s->v = 0;
  s->n = 0;"""

  var numLabels = 0
  val generatedDefs = mutable.ListBuffer[String]()
  val generatedCode = mutable.ListBuffer[String]()

  var numFuncs = 0
  val generatedFuncNames = mutable.Map[Ident, Int]()

  val tempFuncsToGenerate =
    mutable.ListBuffer[(Int, List[(Int, Boolean)], Loop)]()

  def getFunctionName(name: Ident): String = {
    generatedFuncNames.get(name) match {
      case Some(n) => s"f$n"
      case None => {
        val n = numFuncs
        numFuncs += 1
        generatedFuncNames += (name -> n)
        s"f$n"
      }
    }
  }

  def generateBlock(
      instructions: List[Instruction],
      labels: List[(Int, Boolean)]
  ): Unit = {
    instructions match {
      case Nil => {}
      // TODO: Handle integer overflow
      case AddToStackItem(_, n) :: rest => {
        generatedCode += s"p(${n});"
        generateBlock(rest, labels)
      }
      case NewStackItem(_, n) :: rest => {
        generatedCode += s"n(${n});"
        generateBlock(rest, labels)
      }
      case DeferAddToStackItem(_, n) :: rest => {
        generatedCode += s"d(${n},true);"
        generateBlock(rest, labels)
      }
      case DeferNewStackItem(_, n) :: rest => {
        generatedCode += s"d(${n},false);"
        generateBlock(rest, labels)
      }
      case (l @ Loop(_, instructions, _)) :: rest => {
        val fn = numFuncs
        numFuncs += 1
        tempFuncsToGenerate += ((fn, (fn, false) :: labels, l))
        generatedCode += s"f$fn();"
        generateBlock(rest, labels)
      }
      case DeferTailRecLoop(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"i();l${label}:"
        generateBlock(instructions, (label, true) :: labels)
        generatedCode += s"o();"
        generateBlock(rest, labels)
      }
      case TailRecLoop(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"l${label}:"
        generateBlock(instructions, (label, true) :: labels)
        generateBlock(rest, labels)
      }
      case Match(left, _, right) :: rest => {
        generatedCode += s"if(m()){"
        generateBlock(left, labels)
        generatedCode += s"}else{"
        generateBlock(right, labels)
        generatedCode += s"}"
        generateBlock(rest, labels)
      }
      case ResolvedCall(_, name, _) :: rest => {
        generatedCode += s"${getFunctionName(name)}();"
        generateBlock(rest, labels)
      }
      case Call(_, name) :: rest => {
        throw new Exception("Call should have been resolved")
      }
      case Continue(_, i) :: rest => {
        labels(i) match {
          case (l, true)  => generatedCode += s"${"r();" * i}goto l${l};"
          case (f, false) => generatedCode += s"f${f}();"
        }
        generateBlock(rest, labels)
      }
      case Block(_, instructions) :: rest => {
        generateBlock(instructions, labels)
        generateBlock(rest, labels)
      }
      case Print(_) :: rest => {
        generatedCode += s"h();"
        generateBlock(rest, labels)
      }
      case Skip(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"i();l${label}:os();"
        generateBlock(instructions, (label, true) :: labels)
        generatedCode += s"o();"
        generateBlock(rest, labels)
      }
      case Pop(_) :: rest => {
        generatedCode += s"op();"
        generateBlock(rest, labels)
      }
      case Add(_) :: rest => {
        generatedCode += s"oa();"
        generateBlock(rest, labels)
      }
      case Subtract(_) :: rest => {
        generatedCode += s"om();"
        generateBlock(rest, labels)
      }
      case Clone(_) :: rest => {
        generatedCode += s"oc();"
        generateBlock(rest, labels)
      }
      case Multiply(_) :: rest => {
        generatedCode += s"ox();"
        generateBlock(rest, labels)
      }
      case DivMod(_) :: rest => {
        generatedCode += s"od();"
        generateBlock(rest, labels)
      }
      case Swap(_) :: rest => {
        generatedCode += s"ow();"
        generateBlock(rest, labels)
      }
      // case Inspect(_) :: rest => {
      //   generatedCode += s"inspect();"
      //   generateBlock(rest, labels)
      // }
      // TODO: Handle recursive loop
    }
  }

  def generateProgram(program: Program): String = {
    generatedDefs += skeleton
    program.funcs.foreach(f => {
      generatedDefs += s"static void ${getFunctionName(f.name)}();"
      generatedCode += s"static void ${getFunctionName(f.name)}(){"
      generateBlock(f.instructions, List())
      generatedCode += "}"
    })

    generatedCode += mainStart
    generateBlock(program.instructions, List())
    generatedCode += "}"

    while (!tempFuncsToGenerate.isEmpty) {
      val (fn, labels, loop) = tempFuncsToGenerate.remove(0)
      generatedDefs += s"static void f$fn();"
      generatedCode += s"static void f$fn(){"
      generateBlock(loop.contents, labels)
      generatedCode += "}"
    }

    (generatedDefs.toList ::: generatedCode.toList).mkString("")
  }
}
