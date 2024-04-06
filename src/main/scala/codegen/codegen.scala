package codegen

import collection.mutable
import ast._

class codegen {
  val skeleton = """#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
struct frame {
  uint32_t value;
  struct frame *next;
};
struct frame *stack;
struct temp_frame {
  struct frame *top;
  struct frame *bottom;
  bool add;
  struct temp_frame *next;
};
struct temp_frame *temp_stack = 0;
static void pop() {
  if (stack->next == 0) {
    stack->value = 0;
  } else {
    struct frame *f = stack;
    stack = stack->next;
    free(f);
  }
}
static void add(uint32_t x) { stack->value += x; }
static void new(uint32_t x) {
  struct frame *f = malloc(sizeof(struct frame));
  f->value = x;
  f->next = stack;
  stack = f;
}
static void defer(uint32_t x, bool add) {
  if (temp_stack->add) {
    temp_stack->bottom->value += x;
  } else {
    struct frame *f = malloc(sizeof(struct frame));
    f->value = x;
    f->next = 0;
    temp_stack->bottom->next = f;
    temp_stack->bottom = f;
  }
  temp_stack->add = add;
}
static void into() {
  struct temp_frame *t = malloc(sizeof(struct temp_frame));
  struct frame *f = malloc(sizeof(struct frame));
  f->value = 0;
  f->next = 0;
  t->top = f;
  t->bottom = f;
  t->add = true;
  t->next = temp_stack;
  temp_stack = t;
}
static void outof() {
  if (temp_stack->add) {
    temp_stack->bottom->value += stack->value;
    if (stack->next == 0) {
      stack->value = 0;
    } else {
      struct frame *f = stack;
      stack = stack->next;
      free(f);
    }
  }
  temp_stack->bottom->next = stack;
  stack = temp_stack->top;
  struct temp_frame *t = temp_stack;
  temp_stack = temp_stack->next;
  free(t);
}
static bool match() {
  if (stack->value > 0) {
    stack->value--;
    return true;
  } else {
    pop();
    return false;
  }
}
static void merge() {
  struct temp_frame *t = temp_stack->next;
  if (t->add) {
    struct frame *o = temp_stack->top;
    t->bottom->value += o->value;
    if (o->next == 0) {
      o->value = 0;
      temp_stack->add = true;
    } else {
      temp_stack->top = o->next;
      free(o);
    }
  }
  t->add = temp_stack->add;
  t->bottom->next = temp_stack->top;
  t->bottom = temp_stack->bottom;
  struct temp_frame *f = temp_stack;
  temp_stack = t;
  free(f);
}
static void print() {
  uint32_t l = 0;
  struct frame *f = stack;
  while (f != 0 && f->value != 0) {
    l++;
    f = f->next;
  }
  char *b = malloc(l + 1);
  b[l] = 0;
  char *c = b;
  while (stack->value != 0) {
    *c = (char)stack->value;
    c++;
    if (stack->next == 0) {
      stack->value = 0;
      break;
    }
    struct frame *f = stack;
    stack = stack->next;
    free(f);
  }
  pop();
  printf("%s", b);
  free(b);
}
static void read() {
  char *s = NULL;
  size_t bufsize = 0;
  getline(&s, &bufsize, stdin);

  char *c = s;
  while (*c != 0) {
    c++;
  }
  if (*(c - 1) == '\n') {
    c--;
    *c = 0;
  }
  new (0);

  char *q = s;
  while (*q != 0) {
    c--;
    new (*c);
    q++;
  }
  free(s);
}
static void skip() {
  defer(stack->value, false);
  pop();
}
static void skip_o() {
  defer(stack->value, true);
  pop();
}
static void plus() {
  if (stack->next != 0) {
    stack->next->value += stack->value;
    pop();
  }
}
static void mult() {
  if (stack->next != 0) {
    stack->next->value *= stack->value;
    pop();
  } else {
    stack->value = 0;
  }
}
static void divmod() {
  if (stack->next == 0) {
    stack->value = 0;
  } else {
    uint32_t n = stack->next->value;
    uint32_t d = stack->value;
    stack->next->value = n / d;
    stack->value = n % d;
  }
}
static void clone() { new (stack->value); }
static void swap() {
  if (stack->next == 0) {
    new (0);
  } else {
    uint32_t v = stack->next->value;
    stack->next->value = stack->value;
    stack->value = v;
  }
}
static void minus() {
  if (stack->next != 0) {
    if (stack->next->value <= stack->value) {
      stack->next->value = 0;
    } else {
      stack->next->value -= stack->value;
    }
    pop();
  } else {
    stack->value = 0;
  }
}
"""
  val mainStart = """int main(int argc, char *argv[]) {
  stack = malloc(sizeof(struct frame));
  stack->value = 0;
  stack->next = 0;
  for (int i = argc - 1; i >= 0; i--) {
    char *s = argv[i];
    char *c = s;
    while (*c != 0) {
      c++;
    }
    new (0);

    char *q = s;
    while (*q != 0) {
      c--;
      new (*c);
      q++;
    }
  }"""

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
        generatedCode += s"add(${n});"
        generateBlock(rest, labels)
      }
      case NewStackItem(_, n) :: rest => {
        generatedCode += s"new(${n});"
        generateBlock(rest, labels)
      }
      case DeferAddToStackItem(_, n) :: rest => {
        generatedCode += s"defer(${n},true);"
        generateBlock(rest, labels)
      }
      case DeferNewStackItem(_, n) :: rest => {
        generatedCode += s"defer(${n},false);"
        generateBlock(rest, labels)
      }
      case (l @ Loop(_, instructions)) :: rest => {
        val fn = numFuncs
        numFuncs += 1
        tempFuncsToGenerate += ((fn, (fn, false) :: labels, l))
        generatedCode += s"f$fn();"
        generateBlock(rest, labels)
      }
      case DeferTailRecLoop(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"into();l${label}:"
        generateBlock(instructions, (label, true) :: labels)
        generatedCode += s"outof();"
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
        generatedCode += s"if(match()){"
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
          case (l, true)  => generatedCode += s"${"merge();" * i}goto l${l};"
          case (f, false) => generatedCode += s"f${f}();"
        }
        generateBlock(rest, labels)
      }
      case Block(_, instructions) :: rest => {
        generateBlock(instructions, labels)
        generateBlock(rest, labels)
      }
      case Print(_) :: rest => {
        generatedCode += s"print();"
        generateBlock(rest, labels)
      }
      case Read(_) :: rest => {
        generatedCode += s"read();"
        generateBlock(rest, labels)
      }
      case Pop(_) :: rest => {
        generatedCode += s"pop();"
        generateBlock(rest, labels)
      }
      case RecPop(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"l${label}:pop();"
        generateBlock(instructions, (label, true) :: labels)
        generateBlock(rest, labels)
      }
      case DeferPop(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"into();l${label}:pop();"
        generateBlock(instructions, (label, true) :: labels)
        generatedCode += s"outof();"
        generateBlock(rest, labels)
      }
      case Skip(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"into();l${label}:skip();"
        generateBlock(instructions, (label, true) :: labels)
        generatedCode += s"outof();"
        generateBlock(rest, labels)
      }
      case DeferSkip(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"into();l${label}:skip_o();"
        generateBlock(instructions, (label, true) :: labels)
        generatedCode += s"outof();"
        generateBlock(rest, labels)
      }
      case Add(_) :: rest => {
        generatedCode += s"plus();"
        generateBlock(rest, labels)
      }
      case Subtract(_) :: rest => {
        generatedCode += s"minus();"
        generateBlock(rest, labels)
      }
      case Clone(_) :: rest => {
        generatedCode += s"clone();"
        generateBlock(rest, labels)
      }
      case Swap(_) :: rest => {
        generatedCode += s"swap();"
        generateBlock(rest, labels)
      }
      case Multiply(_) :: rest => {
        generatedCode += s"mult();"
        generateBlock(rest, labels)
      }
      case DivMod(_) :: rest => {
        generatedCode += s"divmod();"
        generateBlock(rest, labels)
      }
      case RawC(_, _, fCall) :: rest => {
        generatedCode += s"$fCall();"
        generateBlock(rest, labels)
      }
      // case Inspect(_) :: rest => {
      //   generatedCode += s"inspect();"
      //   generateBlock(rest, labels)
      // }
      // TODO: Handle recursive loop
    }
  }

  def generateProgram(program: Program, cImports: List[String]): String = {
    generatedDefs += skeleton
    cImports.foreach(i => generatedDefs += s"#include \"$i\"\n")
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
