/* $Id: gen_code_stubs.c,v 1.8 2023/03/29 15:42:08 leavens Exp leavens $ */
#include "utilities.h"
#include "gen_code.h"

#include <string.h>
#include <stdbool.h>

// Definining necessary structs and variables for generating code
unsigned int totalProcSize = 1;
int hadConstVar = 0;
int maxNest = 0;

// Procedure code linked list
typedef struct procedure_list{
    const char *name;
    label *addr;
    code_seq block;
    struct procedure_list *next;
    int numNested;
} procedure_list;

procedure_list* head = NULL;

// Initialize the code generator
void gen_code_initialize()
{
    // Replace the following with your implementation
    // bail_with_error("gen_code_initialize not implemented yet!");
}

// Generate code for the given AST
code_seq gen_code_program(AST *prog)
{
    // Generating code blocks for the programs procedures
    code_seq ret = code_seq_empty();
    gen_code_procDecls(prog->data.program.pds);
    if (totalProcSize > 1)
        ret = code_seq_concat(ret, code_jmp(totalProcSize));
    
    // Helps to determines the sequencing of procedure code in the program
    procedure_list *current;
    for (int i = maxNest; i >= 0; i--){
        current = head;
        while (current != NULL)
        {
            if (current->numNested == i)
            {
                ret = code_seq_concat(ret, current->block);
            }
            current = current->next;
        }
    }
    ret = code_seq_concat(ret, code_seq_singleton(code_inc(LINKS_SIZE)));
    ret = code_seq_concat(ret, gen_code_constDecls(prog->data.program.cds));
    ret = code_seq_concat(ret, gen_code_varDecls(prog->data.program.vds));
    ret = code_seq_concat(ret, gen_code_stmt(prog->data.program.stmt));
    code_seq_fix_labels(ret);
    ret = code_seq_add_to_end(ret, code_hlt());
    return ret;
}

// Generates code for blk
code_seq gen_code_block(AST *blk)
{
    gen_code_procDecls(blk->data.program.pds);
    hadConstVar = 0;

    code_seq ret = gen_code_constDecls(blk->data.program.cds);

    ret = code_seq_concat(ret, gen_code_varDecls(blk->data.program.vds));
    
    ret = code_seq_concat(ret, gen_code_stmt(blk->data.program.stmt));
    
    // Checks if the procedure code had any variable or constant declarations
    if (hadConstVar > 0)
        ret = code_seq_concat(ret, code_seq_singleton(code_inc(-hadConstVar)));
    hadConstVar = 0;
    ret = code_seq_concat(ret, code_seq_singleton(code_rtn()));
    return ret;
}

// Generates code for the declarations in cds
code_seq gen_code_constDecls(AST_list cds)
{
    code_seq ret = code_seq_empty();
    while(!ast_list_is_empty(cds))
    {
        ret = code_seq_concat(ret, gen_code_constDecl(ast_list_first(cds)));
        cds = ast_list_rest(cds);
    }
    return ret;
}

// Generates code for the const declaration cd
code_seq gen_code_constDecl(AST *cd)
{
    hadConstVar++;
    return code_seq_singleton(code_lit(cd->data.const_decl.num_val));
}

// Generates code for the declarations in vds
code_seq gen_code_varDecls(AST_list vds)
{
    code_seq ret = code_seq_empty();
    while(!ast_list_is_empty(vds))
    {
        ret = code_seq_concat(ret, gen_code_varDecl(ast_list_first(vds)));
        vds = ast_list_rest(vds);
    }
    return ret;
}

// Generates code for the var declaration vd
code_seq gen_code_varDecl(AST *vd)
{
    hadConstVar++;
    return code_seq_singleton(code_inc(1));
}

// Generates code for the declarations in pds
void gen_code_procDecls(AST_list pds)
{
    while(!ast_list_is_empty(pds))
    {
        gen_code_procDecl(ast_list_first(pds));
        pds = ast_list_rest(pds);
    }
}

// Helper variables for gen_code_procDecl
bool nested = false;
int currentNest = 0;

// Generates code for the procedure declaration pd
void gen_code_procDecl(AST *pd)
{
    procedure_list *proc = (procedure_list*) malloc(sizeof(procedure_list));
    procedure_list *current = head;

    // Checks if the procedure is currently nested inside of another procedure
    if (nested)
    {
        proc->numNested = currentNest + 1;
        maxNest = proc->numNested;
    }
    nested = true;

    // Adds the procedure to the procedure block linked list
    if (head == NULL)
    {
        head = proc;
        proc->numNested = 0;
    }
    else
    {
        while (current->next != NULL)
        {
            current = current->next;
        }
        current->next = proc;
    }

    // Sets the field values of the procedure node
    currentNest = proc->numNested;
    proc->name = pd->data.proc_decl.name;
    proc->addr = pd->data.proc_decl.lab;
    code_seq block = gen_code_block(pd->data.proc_decl.block);
    proc->block = block;
    label_set(proc->addr, totalProcSize);

    // At this point we know the starting address of current procedure
    totalProcSize += code_seq_size(block);
    nested = false;
}

// Generates code for the statement
code_seq gen_code_stmt(AST *stmt)
{
    switch (stmt->type_tag)
    {
        case assign_ast:
            return gen_code_assignStmt(stmt);
            break;
        case call_ast:
            return gen_code_callStmt(stmt);
            break;
        case begin_ast:
            return gen_code_beginStmt(stmt);
            break;
        case if_ast:
            return gen_code_ifStmt(stmt);
            break;
        case while_ast:
            return gen_code_whileStmt(stmt);
        case read_ast:
            return gen_code_readStmt(stmt);
            break;
        case write_ast:
            return gen_code_writeStmt(stmt);
            break;
        case skip_ast:
            return gen_code_skipStmt(stmt);
            break;
        default:
            bail_with_error("Bad AST passed to gen_code_stmt!");
            return code_seq_empty();
    }
}

// Generates code for the assign statement
code_seq gen_code_assignStmt(AST *stmt)
{
    unsigned int outLevels = stmt->data.assign_stmt.ident->data.ident.idu->levelsOutward;
    code_seq ret = code_compute_fp(outLevels);
    ret = code_seq_concat(ret, gen_code_expr(stmt->data.assign_stmt.exp));
    unsigned int ofst = stmt->data.assign_stmt.ident->data.ident.idu->attrs->loc_offset;
    ret = code_seq_add_to_end(ret, code_sto(ofst));
    return ret;
}

// Generates code for the call statement
code_seq gen_code_callStmt(AST *stmt)
{
    const char *name = stmt->data.call_stmt.ident->data.ident.name;
    procedure_list *current = head;
    
    // Finds the procedure associated with the call statement
    while(current != NULL)
    {
        if (strcmp(current->name, name) == 0)
        {
            break;
        }
        current = current->next;
    }
    code_seq ret = code_seq_singleton(code_cal(current->addr));
    return ret;
}

// Generates code for the begin statement
code_seq gen_code_beginStmt(AST *stmt)
{
    AST_list stmts = stmt->data.begin_stmt.stmts;
    code_seq ret = code_seq_singleton(gen_code_stmt(ast_list_first(stmts)));
    stmts = ast_list_rest(stmts);
    while (!ast_list_is_empty(stmts)) 
    {
        ret = code_seq_concat(ret, gen_code_stmt(ast_list_first(stmts)));
        stmts = ast_list_rest(stmts);
    }
    return ret;
}

// Generates code for the if statement
code_seq gen_code_ifStmt(AST *stmt)
{
    code_seq condc = gen_code_cond(stmt->data.if_stmt.cond);
    code_seq ret = code_seq_add_to_end(condc, code_jpc(2));
    code_seq thenc = gen_code_stmt(stmt->data.if_stmt.thenstmt);
    code_seq elsec = gen_code_stmt(stmt->data.if_stmt.elsestmt);
    ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(thenc)+2));
    ret = code_seq_concat(ret, thenc);
    ret = code_seq_add_to_end(ret, code_jmp(code_seq_size(elsec)+1));
    ret = code_seq_concat(ret, elsec);
    return ret;
}

// Generate codes for the while statement
code_seq gen_code_whileStmt(AST *stmt)
{
    code_seq ret = gen_code_cond(stmt->data.while_stmt.cond);
    unsigned int condSize = code_seq_size(ret);
    ret = code_seq_add_to_end(ret, code_jpc(2));
    code_seq bodyc = gen_code_stmt(stmt->data.while_stmt.stmt);
    unsigned int bodySize = code_seq_size(bodyc);
    ret = code_seq_add_to_end(ret, code_jmp(bodySize + 2));
    ret = code_seq_concat(ret, bodyc);
    ret = code_seq_add_to_end(ret, code_jmp(-1 * (bodySize + condSize + 2)));
    return ret;
}

// Generates code for the read statement
code_seq gen_code_readStmt(AST *stmt)
{
    id_use *idu = stmt->data.read_stmt.ident->data.ident.idu;
    code_seq ret = code_compute_fp(idu->levelsOutward);
    ret = code_seq_add_to_end(ret, code_chi());
    ret = code_seq_add_to_end(ret, code_sto(idu->attrs->loc_offset));
    return ret;
}

// Generates code for the write statement
code_seq gen_code_writeStmt(AST *stmt)
{
    code_seq ret = gen_code_expr(stmt->data.write_stmt.exp);
    ret = code_seq_add_to_end(ret, code_cho());
    return ret;
}

// Generates code for the skip statement
code_seq gen_code_skipStmt(AST *stmt)
{
    code_seq ret = code_nop();
    return ret;
}

// Generates code for the condition
code_seq gen_code_cond(AST *cond)
{
    switch (cond->type_tag)
    {
        case odd_cond_ast:
            return gen_code_odd_cond(cond);
            break;
        case bin_cond_ast:
            return gen_code_bin_cond(cond);
            break;
        default:
            bail_with_error("Bad AST passed to gen_code_stmt!");
            return code_seq_empty();
    }
}

// Generates code for the odd condition
code_seq gen_code_odd_cond(AST *cond)
{
    code_seq ret = gen_code_expr(cond->data.odd_cond.exp);
    ret = code_seq_add_to_end(ret, code_lit(2));
    ret = code_seq_add_to_end(ret, code_mod());
    return ret;
}

// Generate code for the bin condition
code_seq gen_code_bin_cond(AST *cond)
{
    code_seq ret = gen_code_expr(cond->data.bin_expr.leftexp);
    ret = code_seq_concat(ret, gen_code_expr(cond->data.bin_expr.rightexp));
    switch (cond->data.bin_cond.relop) {
    case eqop:
    return code_seq_add_to_end(ret, code_eql());
    break;
    case neqop:
    return code_seq_add_to_end(ret, code_neq());
    break;
    case ltop:
    return code_seq_add_to_end(ret, code_lss());
    break;
    case leqop:
    return code_seq_add_to_end(ret, code_leq());
    break;
    case gtop:
    return code_seq_add_to_end(ret, code_gtr());
    break;
    case geqop:
    return code_seq_add_to_end(ret, code_geq());
    break;
    default:
    bail_with_error("gen_code_bin_expr passed AST with bad op!");
    // The following should never execute
    return code_seq_empty();
    }
}

// Generates code for the expresion exp
code_seq gen_code_expr(AST *exp)
{
    switch (exp->type_tag) {
    case number_ast:
    return gen_code_number_expr(exp);
    break;
    case ident_ast:
    return gen_code_ident_expr(exp);
    break;
    case bin_expr_ast:
    return gen_code_bin_expr(exp);
    break;
    default:
    bail_with_error("gen_code_expr passed bad AST!");
    // The following should never execute
    return code_seq_empty();
    }
}

// Generates code for the bin expression exp
code_seq gen_code_bin_expr(AST *exp)
{
    code_seq ret = gen_code_expr(exp->data.bin_expr.leftexp);
    ret = code_seq_concat(ret, gen_code_expr(exp->data.bin_expr.rightexp));
    switch (exp->data.bin_expr.arith_op) {
    case addop:
    return code_seq_add_to_end(ret, code_add());
    break;
    case subop:
    return code_seq_add_to_end(ret, code_sub());
    break;
    case multop:
    return code_seq_add_to_end(ret, code_mul());
    break;
    case divop:
    return code_seq_add_to_end(ret, code_div());
    break;
    default:
    bail_with_error("gen_code_bin_expr passed AST with bad op!");
    // The following should never execute
    return code_seq_empty();
    }
}

// Generates code for the ident expression ident
code_seq gen_code_ident_expr(AST *ident)
{
    id_use *idu = ident->data.ident.idu;
    lexical_address *la = lexical_address_create(idu->levelsOutward,idu->attrs->loc_offset);
    return code_load_from_lexical_address(la);
}

// Generates code for the number expression num
code_seq gen_code_number_expr(AST *num)
{
    return code_seq_singleton(code_lit(num->data.number.value));
}
