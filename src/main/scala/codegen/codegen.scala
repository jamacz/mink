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
static void pop_stack() {
  if (stack->next == 0) {
    stack->value = 0;
  } else {
    struct frame *f = stack;
    stack = stack->next;
    free(f);
  }
}
static void add_stack(uint32_t x) { stack->value += x; }
static void new_stack(uint32_t x) {
  struct frame *f = malloc(sizeof(struct frame));
  f->value = x;
  f->next = stack;
  stack = f;
}
static void defer_stack(uint32_t x, bool add) {
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
static void enter_loop() {
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
static void exit_loop() {
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
static bool match_stack() {
  if (stack->value > 0) {
    stack->value--;
    return true;
  } else {
    pop_stack();
    return false;
  }
}
static void merge_stacks() {
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
static void print_stack() {
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
  pop_stack();
  printf("%s", b);
  free(b);
}
static void read_stack() {
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
  new_stack(0);

  char *q = s;
  while (*q != 0) {
    c--;
    new_stack(*c);
    q++;
  }
  free(s);
}
static void skip_stack() {
  defer_stack(stack->value, false);
  pop_stack();
}
static void skip_ones() {
  defer_stack(stack->value, true);
  pop_stack();
}
static void plus_stack() {
  if (stack->next != 0) {
    stack->next->value += stack->value;
    pop_stack();
  }
}
static void mult_stack() {
  if (stack->next != 0) {
    stack->next->value *= stack->value;
    pop_stack();
  } else {
    stack->value = 0;
  }
}
static void divmod_stack() {
  if (stack->next == 0) {
    stack->value = 0;
  } else {
    uint32_t n = stack->next->value;
    uint32_t d = stack->value;
    stack->next->value = n / d;
    stack->value = n % d;
  }
}
static void clone_stack() { new_stack(stack->value); }
static void swap_stack() {
  if (stack->next == 0) {
    new_stack(0);
  } else {
    uint32_t v = stack->next->value;
    stack->next->value = stack->value;
    stack->value = v;
  }
}
static void minus_stack() {
  if (stack->next != 0) {
    if (stack->next->value <= stack->value) {
      stack->next->value = 0;
    } else {
      stack->next->value -= stack->value;
    }
    pop_stack();
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
    new_stack(0);

    char *q = s;
    while (*q != 0) {
      c--;
      new_stack(*c);
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
        generatedCode += s"add_stack(${n});"
        generateBlock(rest, labels)
      }
      case NewStackItem(_, n) :: rest => {
        generatedCode += s"new_stack(${n});"
        generateBlock(rest, labels)
      }
      case DeferAddToStackItem(_, n) :: rest => {
        generatedCode += s"defer_stack(${n},true);"
        generateBlock(rest, labels)
      }
      case DeferNewStackItem(_, n) :: rest => {
        generatedCode += s"defer_stack(${n},false);"
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
        generatedCode += s"enter_loop();l${label}:"
        generateBlock(instructions, (label, true) :: labels)
        generatedCode += s"exit_loop();"
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
        generatedCode += s"if(match_stack()){"
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
          case (l, true) =>
            generatedCode += s"${"merge_stacks();" * i}goto l${l};"
          case (f, false) => generatedCode += s"f${f}();"
        }
        generateBlock(rest, labels)
      }
      case Block(_, instructions) :: rest => {
        generateBlock(instructions, labels)
        generateBlock(rest, labels)
      }
      case Print(_) :: rest => {
        generatedCode += s"print_stack();"
        generateBlock(rest, labels)
      }
      case Read(_) :: rest => {
        generatedCode += s"read_stack();"
        generateBlock(rest, labels)
      }
      case Pop(_) :: rest => {
        generatedCode += s"pop_stack();"
        generateBlock(rest, labels)
      }
      case RecPop(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"l${label}:pop_stack();"
        generateBlock(instructions, (label, true) :: labels)
        generateBlock(rest, labels)
      }
      case DeferPop(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"enter_loop();l${label}:pop_stack();"
        generateBlock(instructions, (label, true) :: labels)
        generatedCode += s"exit_loop();"
        generateBlock(rest, labels)
      }
      case Skip(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"enter_loop();l${label}:skip_stack();"
        generateBlock(instructions, (label, true) :: labels)
        generatedCode += s"exit_loop();"
        generateBlock(rest, labels)
      }
      case DeferSkip(_, instructions) :: rest => {
        val label = numLabels
        numLabels += 1
        generatedCode += s"enter_loop();l${label}:skip_ones();"
        generateBlock(instructions, (label, true) :: labels)
        generatedCode += s"exit_loop();"
        generateBlock(rest, labels)
      }
      case Add(_) :: rest => {
        generatedCode += s"plus_stack();"
        generateBlock(rest, labels)
      }
      case Subtract(_) :: rest => {
        generatedCode += s"minus_stack();"
        generateBlock(rest, labels)
      }
      case Clone(_) :: rest => {
        generatedCode += s"clone_stack();"
        generateBlock(rest, labels)
      }
      case Swap(_) :: rest => {
        generatedCode += s"swap_stack();"
        generateBlock(rest, labels)
      }
      case Multiply(_) :: rest => {
        generatedCode += s"mult_stack();"
        generateBlock(rest, labels)
      }
      case DivMod(_) :: rest => {
        generatedCode += s"divmod_stack();"
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
