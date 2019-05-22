#include "cgen.h"
#include "dast.h"
#include "instructions.h"
#include "decorate.h"
#include "utils.h"
#include "cgen-helpers.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


string_label_map* s = NULL;

void GenerateCode(DAST* dast, char* filename) {
  Decl** globals = global->children;
  int global_len = global->children_size;
  // indicate we expect a main function
  emitGLOBLMAIN ();
  // set up globals (vars, strings)
  emitDATA ();
  // First sort data based on offset
  for (int i = 0; i < global_len; i++) {
    Decl* curr = globals[i];
    // Allocating for a string requires a label
    if (curr->group == DECL_STR) {
      // if we're declaring a string
      char* label = generateStringLabel(curr->contents.str_info.str_literal);
      emitLABEL(label);
      generateString(curr);
    // Allocating for a global variable just requires initializing a value
    } else if (curr->group == DECL_VAR) {
      if (curr->is_string) {
        // if our global var has a str value
        generateDataLabel (generateStringLabel (curr->contents.str_info.str_literal), curr->data_size);
      } else {
        // if any other value
        int64_t value = 0;
        for (int k = WORDSIZE; k >= 0; k--) {
          value = value << 8;
          value += curr->contents.value[k];
        }
        generateDataValue (value, curr->data_size);
      }
    }
  }
  emitTEXT ();
  dispatch(dast, NULL, NULL);

  freeStringList(s);
}

void processDefault(DAST* dast, char* startLabel, char* endLabel) {
  for (int i = 0; i < dast->size; i++) {
    dispatch(dast->children[i], startLabel, endLabel);
  }
}

void processID(DAST* dast, char* startLabel, char* endLabel) {
  enum DeclLevel level = dast->decl->level;
  int offset = dast->decl->offset;
  if (level == FUNCTION) {
    emitADDI(S1, FP, offset);
    emitLW(S1, 0, S1);
  } else if (level == GLOBAL) {
    emitADDI(S1, GP, offset);
    emitLW(S1, 0, S1);
  }
  // struct processing is done in dot and arrow expr
}

void processConstTrue(DAST* dast, char* startLabel, char* endLabel) {
  emitADDI(S1, x0, 1);
}

void processConstFalse(DAST* dast,
                       char* startLabel,
                       char* endLabel) {
  emitADDI(S1, x0, 0);
}

void processConstInt(DAST* dast, char* startLabel, char* endLabel) {
  emitADDI(S1, x0, dast->data.integer);
}

void processConstNull(DAST* dast, char* startLabel, char* endLabel) {
  emitADDI(S1, x0, 0);
}

void processConstChar(DAST* dast, char* startLabel, char* endLabel) {
  char c = dast->data.character;
  emitADDI(S1, x0, (int)c);
}

void processConstStr(DAST* dast, char* startLabel, char* endLabel) {
  emitLA(S1, getStringLabel(s, dast->data.string));
}

void processExprBinaryAdd(DAST* dast,
                          char* startLabel,
                          char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  /* our left expr result is now in S1! but... if we call dispatch on child2, 
 * we'll overwrite the value from child1... */

  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack

  /* S1 is free to use */
  dispatch(child2, startLabel, endLabel); // S1 holds right expr result
  
  emitLW(T0, 0, SP); // load left expr result from stack
  emitADD(S1, S1, T0); // add values and place result in S1 to keep invariant true

  emitADDI(SP, SP, 4); // erase value from stack/restore pointer
  
}

void processExprBinarySub(DAST* dast,
                          char* startLabel,
                          char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  /* our left expr result is now in S1! but... if we call dispatch on child2, 
 * we'll overwrite the value from child1... */

  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack

  /* S1 is free to use */
  dispatch(child2, startLabel, endLabel); // S1 holds right expr result

  emitLW(T0, 0, SP); // load left expr result from stack
  emitSUB(S1, T0, S1); // add values and place result in S1 to keep invariant true

  emitADDI(SP, SP, 4); // erase value from stack/restore pointer
}

void processExprBinaryMul(DAST* dast,
                          char* startLabel,
                          char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  /* our left expr result is now in S1! but... if we call dispatch on child2, 
 * we'll overwrite the value from child1... */

  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack

  /* S1 is free to use */
  dispatch(child2, startLabel, endLabel); // S1 holds right expr result

  emitLW(T0, 0, SP); // load left expr result from stack
  emitMUL(S1, S1, T0); // add values and place result in S1 to keep invariant true

  emitADDI(SP, SP, 4); // erase value from stack/restore pointer
}

void processExprBinaryDiv(DAST* dast,
                          char* startLabel,
                          char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  /* our left expr result is now in S1! but... if we call dispatch on child2, 
 * we'll overwrite the value from child1... */

  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack

  /* S1 is free to use */
  dispatch(child2, startLabel, endLabel); // S1 holds right expr result

  emitLW(T0, 0, SP); // load left expr result from stack
  emitDIV(S1, T0, S1); // add values and place result in S1 to keep invariant true

  emitADDI(SP, SP, 4); // erase value from stack/restore pointer
}

void processExprBinaryEq(DAST* dast,
                         char* startLabel,
                         char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);

  emitADDI(SP, SP, -4);
  emitSW(S1, 0, SP);

  dispatch(child2, startLabel, endLabel);

  emitLW(T0, 0, SP);
  emitXOR(T1, T0, S1); //check if all bits the same
  emitSLTIU(S1, T1, 1); // if result of xor == 0, then they are equal, set s1 to 1 
  emitADDI(SP, SP, 4);
}

void processExprBinaryNotEq(DAST* dast,
                            char* startLabel,
                            char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);

  emitADDI(SP, SP, -4);
  emitSW(S1, 0, SP);

  dispatch(child2, startLabel, endLabel);

  emitLW(T0, 0, SP);
  emitXOR(T1, T0, S1); // check if all bits the same
  emitSLTIU(S1, T1, 1); 
  emitXORI(S1, S1, 1); // do bitwise not on result above to get if they are not equal

  emitADDI(SP, SP, 4);
}

void processExprBinaryGTEq(DAST* dast,
                           char* startLabel,
                           char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  
  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack
  
  dispatch(child2, startLabel, endLabel);

  emitLW(T0, 0, SP);
  emitSLT(S1, T0, S1); // check less than
  emitXORI(S1, S1, 1); // !less than = geq

  emitADDI(SP, SP, 4);
  
}

void processExprBinaryGT(DAST* dast,
                         char* startLabel,
                         char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  
  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack
  
  dispatch(child2, startLabel, endLabel);

  emitLW(T0, 0, SP);
  emitSLT(S1, S1, T0);
  
  emitADDI(SP, SP, 4);

}

void processExprBinaryLTEq(DAST* dast,
                           char* startLabel,
                           char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  
  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack
  
  dispatch(child2, startLabel, endLabel);

  emitLW(T0, 0, SP);
  emitSLT(S1, S1, T0); // check greater than
  emitXORI(S1, S1, 1); // !greater than = leq
  
  emitADDI(SP, SP, 4); 
}

void processExprBinaryLT(DAST* dast,
                         char* startLabel,
                         char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  
  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack
  
  dispatch(child2, startLabel, endLabel);

  emitLW(T0, 0, SP);
  emitSLT(S1, T0, S1);
  
  emitADDI(SP, SP, 4);
}

void processExprBinaryLogicAnd(DAST* dast,
                               char* startLabel,
                               char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  
  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack
  
  dispatch(child2, startLabel, endLabel);

  emitLW(T0, 0, SP);

  emitSLT(T1, x0, T0); // see if arg1 > 0
  emitSLT(T2, T0, x0); // see if arg1 < 0
  emitOR(T0, T1, T2); // see if arg1 != 0

  emitSLT(T3, x0, S1); // see if arg2 > 0
  emitSLT(T4, S1, x0); // see if arg2 < 0
  emitOR(S1, T3, T4); // see if arg2 != 0

  emitAND(S1, S1, T0); // check if arg1 AND arg2 != 0
}

void processExprBinaryLogicOr(DAST* dast,
                              char* startLabel,
                              char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  
  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack
  
  dispatch(child2, startLabel, endLabel);

  emitLW(T0, 0, SP);

  emitSLT(T1, x0, T0); // see if arg1 > 0
  emitSLT(T2, T0, x0); // see if arg1 < 0
  emitOR(T0, T1, T2); // see if arg1 != 0

  emitSLT(T3, x0, S1); // see if arg2 > 0
  emitSLT(T4, S1, x0); // see if arg2 < 0
  emitOR(S1, T3, T4); // see if arg2 != 0

  emitOR(S1, S1, T0); // check if arg1 OR arg2 != 0
}

void processExprBinaryBitAnd(DAST* dast,
                             char* startLabel,
                             char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  
  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack
  
  dispatch(child2, startLabel, endLabel);

  emitLW(T0, 0, SP);
  emitAND(S1, T0, S1);

  emitADDI(SP, SP, 4);
}

void processExprBinaryBitOr(DAST* dast,
                            char* startLabel,
                            char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
  
  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack
  
  dispatch(child2, startLabel, endLabel);

  emitLW(T0, 0, SP);
  emitOR(S1, T0, S1);

  emitADDI(SP, SP, 4);
}

void processExprBinaryBitXor(DAST* dast,
                             char* startLabel,
                             char* endLabel) {
  DAST* child1 = dast->children[0]; // left expression
  DAST* child2 = dast->children[1]; // right expression

  dispatch(child1, startLabel, endLabel);
 
  emitADDI(SP, SP, -4); // make room on the stack/move pointer
  emitSW(S1, 0, SP); // save value to stack
  
  dispatch(child2, startLabel, endLabel);

  emitLW(T0, 0, SP);
  emitXOR(S1, T0, S1);

  emitADDI(SP, SP, 4);
}

void processExprPrefixNegate(DAST* dast,
                             char* startLabel,
                             char* endLabel) {
  DAST* child1 = dast->children[0];

  // cgen and load arguments
  dispatch(child1, startLabel, endLabel);
  // negate
  emitADDI(T0, x0, -1);
  emitMUL(S1, S1, T0);
}

void processExprPrefixPlusPlus(DAST* dast, char* startLabel, char* endLabel) {
  // Fetch variable's address
  dispatchLeft (dast->children[0], startLabel, endLabel);

  // Move address into s2
  emitMV (S2, S1);
  
  // Load and change value
  emitLW (S1, 0, S2);
  emitADDI (S1, S1, 1);

  // Store the new value
  emitSW (S1, 0, S2);
}

void processExprPrefixMinusMinus(DAST* dast, char* startLabel, char* endLabel) {
  // Fetch variable's address
  dispatchLeft (dast->children[0], startLabel, endLabel);

  // Move address into s2
  emitMV (S2, S1);
  
  // Load and change value
  emitLW (S1, 0, S2);
  emitADDI (S1, S1, -1);

  // Store the new value
  emitSW (S1, 0, S2);
}

void processExprPrefixBitNot(DAST* dast,
                             char* startLabel,
                             char* endLabel) {
  DAST* child1 = dast->children[0];

  dispatch(child1, startLabel, endLabel);
  // do bitwise negate
  emitXORI(S1, S1, -1);  // get all ones mask
}

void processExprPrefixLogicNot(DAST* dast,
                               char* startLabel,
                               char* endLabel) {
  DAST* child1 = dast->children[0];

  dispatch(child1, startLabel, endLabel);

  emitSLTIU(
      S1, S1,
      1);  // return if the value is less than 1 unsigned aka 1 if 0 else 0
}

void processExprPostfixPlusPlus(DAST* dast, char* startLabel, char* endLabel) {
  // Fetch variable's address
  dispatchLeft (dast->children[0], startLabel, endLabel);

  // Move address into s2
  emitMV (S2, S1);
  
  // Load and change value
  emitLW (S1, 0, S2);
  emitADDI (S3, S1, 1);

  // Store the new value
  emitSW (S3, 0, S2);
}

void processExprPostfixMinusMinus(DAST* dast, char* startLabel, char* endLabel) {
  // Fetch variable's address
  dispatchLeft (dast->children[0], startLabel, endLabel);

  // Move address into s2
  emitMV (S2, S1);
  
  // Load and change value
  emitLW (S1, 0, S2);
  emitADDI (S3, S1, -1);

  // Store the new value
  emitSW (S3, 0, S2);
}

void processFuncDecl(DAST* dast, char* startLabel, char* endLabel) {
  if (dast->size == 4) {
    // only cgen on functions with bodies
    DAST* func_id = dast->children[1];
    DAST* func_body = dast->children[3];

    emitLABEL(func_id->data.identifier);

    /*
      YOUR CODE HERE
      - Prologue
      - Generate code for function body
      - Set up stack and frame
    */
    emitADDI(SP, SP, -4);
    emitSW(FP, 0, SP); // store FP in there
    emitMV(FP, SP); // set FP to current SP (right below old_fp
   
    emitADDI(SP, SP, -48); // Allocate space on stack for saved registers

    emitSW(RA, -4, FP); //save registers according to FP
    emitSW(S11, -8, FP);
    emitSW(S10, -12, FP);
    emitSW(S9, -16, FP);
    emitSW(S8, -20, FP);
    emitSW(S7, -24, FP);
    emitSW(S6, -28, FP);
    emitSW(S5, -32, FP);
    emitSW(S4, -36, FP);
    emitSW(S3, -40, FP);
    emitSW(S2, -44, FP);
    emitSW(S1, -48, FP); 

    dispatch(func_body, startLabel, endLabel); // generate code for function body

    // produce a label for return statements to come back to
    char *total_string = generateFunctionEndLabel (func_id->data.identifier);
    emitLABEL (total_string);
    free (total_string);

    /*
      YOUR CODE HERE
      - Epilogue
      - Restore stack and frame
    */

    emitLW(S1, -48, FP);
    emitLW(S2, -44, FP);
    emitLW(S3, -40, FP);
    emitLW(S4, -36, FP);
    emitLW(S5, -32, FP);
    emitLW(S6, -28, FP);
    emitLW(S7, -24, FP);
    emitLW(S8, -20, FP);
    emitLW(S9, -16, FP);
    emitLW(S10, -12, FP);
    emitLW(S11, -8, FP);
    emitLW(RA, -4, FP);
    
    emitLW(FP, 0, FP); //restore old fp
    
    emitADDI(SP, SP, 52);

    if (strcmp ("main", func_id->data.identifier) == 0) {
      // If we are the main function we want to exit to
      // be compatible with venus.
      emitMV (A1, A0);
      emitADDI (A0, x0, 17);
      emitECALL ();
    } else {
      // Jump out of the function
      emitJR(RA);
    }
  }
}

// TODO EXPR ARGS iteration
void processExprCall(DAST* dast, char* startLabel, char* endLabel) {
  DAST* expr_id = dast->children[0];
  DAST* expr_args = dast->children[1];

  
  emitADDI(SP, SP, -(expr_args->size * 4));
  for (int i = 0; i < expr_args->size; i++) {
    dispatch(expr_args->children[i], startLabel, endLabel);
    emitSW(S1, i * 4, SP); 
  }

  emitJAL(RA, expr_id->data.identifier);

  emitMV(S1, A0);
  
  emitADDI(SP, SP, (expr_args->size*4));
}

void processBlock(DAST* dast, char* startLabel, char* endLabel) {
  // figure out how much to move stack by
  int bytes_used = dast->decl->data_size;
  if (bytes_used > 0) {
    emitADDI(SP, SP, -1 * bytes_used);
  }

  // dispatch on all children
  for (int i = 0; i < dast->size; i++) {
    dispatch(dast->children[i], startLabel, endLabel);
  }
  if (bytes_used > 0) {
    emitADDI(SP, SP, bytes_used);
  }
}

// TODO if else
void processIfElse(DAST* dast, char* startLabel, char* endLabel) {
  int has_else = dast->size > 2;

  DAST* condition = dast->children[0]; //condition block
  DAST* if_block = dast->children[1]; // if block
  char* outside_if = generateLocalLabel();

  dispatch(condition, startLabel, endLabel); // see if condition is true
  
  if (has_else) {
    char* else_label = generateLocalLabel(); //else label
    DAST* else_block = dast->children[2];

    emitBEQ(S1, x0, else_label); // if condition == 0, jump to else label
    dispatch(if_block, startLabel, endLabel); // do if statement
    emitJ(outside_if); // jump to outside if
    emitLABEL(else_label); // else label
    dispatch(else_block, startLabel, endLabel); //do else label
    emitLABEL(outside_if); //outside if label
  } else {
    emitBEQ(S1, x0, outside_if);
    dispatch(if_block, startLabel, endLabel);
    emitLABEL(outside_if);
  }
}

void processFor(DAST* dast, char* startLabel, char* endLabel) {
  char* start_for = generateLocalLabel();
  char* middle_for = generateLocalLabel();
  char* end_for = generateLocalLabel();

  // Initialize condition
  dispatch(dast->children[0], start_for, end_for);

  // Jump to condition
  emitJ(middle_for);

  // Label for start of the loop
  emitLABEL(start_for);

  //Update condition on loop
  dispatch(dast->children[2], start_for, end_for);

  // Label for cond. This is to skip first update
  emitLABEL(middle_for);

  // Loop condition bounding
  dispatch(dast->children[1], start_for, end_for);
  emitBEQZ (S1, end_for);
  dispatch(dast->children[3], start_for, end_for);

  // Jump back to update
  emitJ (start_for);

  emitLABEL(end_for);

}

void processVarDecl(DAST* dast, char* startLabel, char* endLabel) {
  // Only need to initialize a var if it has an expr
  if (dast->size > 3 && dast->decl->level == FUNCTION) {
    // dispatch on expr
    dispatch(dast->children[3], startLabel, endLabel);

    int offset = dast->decl->offset;
    emitSW(S1, offset, FP);
  }
}

void processReturn(DAST* dast, char* startLabel, char* endLabel) {
  dispatch(dast->children[0], startLabel, endLabel);
  emitMV(A0, S1);
  // Generate the return label for the function
  char *total_string = generateFunctionEndLabel (dast->decl->identifier);
  // Jump to the return label
  emitJ(total_string);
  free(total_string);
}

void processExprAssign(DAST* dast,
                       char* startLabel,
                       char* endLabel) {
  DAST* LHS = dast->children[0];
  DAST* RHS = dast->children[1];

  // get address to store at
  dispatchLeft(LHS, startLabel, endLabel);

  emitADDI(SP, SP, -1 * WORDSIZE);
  emitSW(S1, 0, SP);
  // get value
  dispatch(RHS, startLabel, endLabel);
  emitLW(T0, 0, SP);

  // store value in expr location
  emitSW(S1, 0, T0);
  emitADDI(SP, SP, WORDSIZE);
}

void processAddr(DAST* dast, char* startLabel, char* endLabel) {

  // treat as LHS expr to get address
  dispatchLeft(dast->children[0], startLabel, endLabel);
}

void processDeref(DAST *dast, char* startLabel, char* endLabel) {
  // Evaluate expression
  dispatch(dast->children[0], startLabel, endLabel);
  
  // Dereference that pointer
  emitLW (S1, 0, S1);
}

void processDot(DAST* dast, char* startLabel, char* endLabel) {
  dispatchLeft(dast, startLabel, endLabel);
  // we have gotten the address of our field, now use to load
  emitLW(S1, 0, S1);
}

void processArrow(DAST* dast, char* startLabel, char* endLabel) {
  dispatchLeft(dast, startLabel, endLabel);
  // we have gotten the address of our field, now use to load
  emitLW(S1, 0, S1);
}

void processBreak(DAST* dast, char* startLabel, char* endLabel) {
  emitJ(endLabel);
}

void processContinue(DAST* dast, char* startLabel, char* endLabel) {
  emitJ(startLabel);
}

void dispatch(DAST* dast, char* startLabel, char* endLabel) {
  switch (dast->type) {
    case (NODETYPE_CONSTANT_INTEGER):
      processConstInt(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONSTANT_STRING):
      processConstStr(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONSTANT_CHARACTER):
      processConstChar(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONSTANT_TRUE):
      processConstTrue(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONSTANT_FALSE):
      processConstFalse(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONSTANT_NULL):
      processConstNull(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONTROL_IF_ELSE):
      processIfElse(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONTROL_FOR):
      processFor(dast, startLabel, endLabel);
      break;
    case (NODETYPE_VAR_DECL):
      processVarDecl(dast, startLabel, endLabel);
      break;
    case (NODETYPE_FUNC_DECL):
      processFuncDecl(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_ADD):
      processExprBinaryAdd(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_SUB):
      processExprBinarySub(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_MULT):
      processExprBinaryMul(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_DIV):
      processExprBinaryDiv(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_EQUAL):
      processExprBinaryEq(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_NOTEQUAL):
      processExprBinaryNotEq(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_GREATEREQUAL):
      processExprBinaryGTEq(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_GREATER):
      processExprBinaryGT(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_LESSEQUAL):
      processExprBinaryLTEq(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_LESS):
      processExprBinaryLT(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_ANDAND):
      processExprBinaryLogicAnd(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_OROR):
      processExprBinaryLogicOr(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_AND):
      processExprBinaryBitAnd(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_OR):
      processExprBinaryBitOr(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_XOR):
      processExprBinaryBitXor(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_PLUSPLUS):
      processExprPrefixPlusPlus(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_MINUSMINUS):
      processExprPrefixMinusMinus(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_NEGATE):
      processExprPrefixNegate(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_DEREFERENCE):
      processDeref(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_ADDRESS):
      processAddr(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_NOTNOT):
      processExprPrefixLogicNot(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_NOT):
      processExprPrefixBitNot(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_POSTFIX_PLUSPLUS):
      processExprPostfixPlusPlus(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_POSTFIX_MINUSMINUS):
      processExprPostfixMinusMinus(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_STRUCT_DOT):
      processDot(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_STRUCT_ARROW):
      processArrow(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_CALL):
      processExprCall(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_ASSIGN):
      processExprAssign(dast, startLabel, endLabel);
      break;
    case (NODETYPE_ID):
      processID(dast, startLabel, endLabel);
      break;
    case (NODETYPE_BREAK):
      processBreak(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONTINUE):
      processContinue(dast, startLabel, endLabel);
      break;
    case (NODETYPE_RETURN):
      processReturn(dast, startLabel, endLabel);
      break;
    case (NODETYPE_BLOCK):
      processBlock(dast, startLabel, endLabel);
      break;
    case (NODETYPE_STRUCT_DEF):
      break;
    default:
      processDefault(dast, startLabel, endLabel);
      break;
  }
}

void processLeftID(DAST* dast, char* startLabel, char* endLabel) {
  enum DeclLevel level = dast->decl->level;
  int offset = dast->decl->offset;
  if (level == FUNCTION) {
    emitADDI(S1, FP, offset);
  } else if (level == GLOBAL) {
    emitADDI(S1, GP, offset);
  }
}

void processLeftPren(DAST* dast, char* startLabel, char* endLabel) {
  // This seems like it would work in theory....
  dispatchLeft(dast->children[0], startLabel, endLabel);
}

void processLeftDeref(DAST* dast, char* startLabel, char* endLabel) {
  // get address of variable in memory
  dispatch(dast->children[0], startLabel, endLabel);
}

void processLeftDot(DAST* dast, char* startLabel, char* endLabel) {
  DAST* struct_instance = dast->children[0];
  // get the address of the struct instance
  dispatchLeft(struct_instance, startLabel, endLabel);

  int offset = dast->children[1]->decl->offset;
  // get addr of struct field
  emitADDI(S1, S1, offset);
}

void processLeftArrow(DAST* dast, char* startLabel, char* endLabel) {
  DAST* struct_instance = dast->children[0];
  // get the address of the struct instance
  dispatch(struct_instance, startLabel, endLabel);

  int offset = dast->children[1]->decl->offset;
  // get addr of struct field
  emitADDI(S1, S1, offset);
}

void dispatchLeft(DAST* dast, char* startLabel, char* endLabel) {
  switch (dast->type) {
    case (NODETYPE_EXPR_STRUCT_DOT):
      processLeftDot(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_STRUCT_ARROW):
      processLeftArrow(dast, startLabel, endLabel);
      break;
    case (NODETYPE_ID):
      processLeftID(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREN):
      processLeftPren(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_DEREFERENCE):
      processLeftDeref(dast, startLabel, endLabel);
      break;
    default:
      dispatch(dast, startLabel, endLabel);
      break;
  }
}
