#include "Ast.h"
#include <iostream>
#include <stack>
#include <string>
#include "IRBuilder.h"
#include "Instruction.h"
#include "SymbolTable.h"
#include "Unit.h"
#include "MachineCode.h"

extern Unit unit;
extern MachineUnit mUnit;
extern FILE* yyout;
int Node::counter = 0;
IRBuilder* Node::builder;

Node::Node()
{
    seq = counter++;
    next = nullptr;
}

void Node::backPatch(std::vector<Instruction*>& list, BasicBlock* bb)
{
    for (auto& inst : list) 
    {
        if (inst->isCond())
            dynamic_cast<CondBrInstruction*>(inst)->setTrueBranch(bb);
        else if (inst->isUncond())
            dynamic_cast<UncondBrInstruction*>(inst)->setBranch(bb);
    }
}

std::vector<Instruction*> Node::merge(std::vector<Instruction*>& list1, std::vector<Instruction*>& list2) 
{
    std::vector<Instruction*> res(list1);
    res.insert(res.end(), list2.begin(), list2.end());
    return res;
}

void Node::setNext(Node* node)
{
    Node* n = this;
    while (n->getNext())
        n = n->getNext();
    if (n == this)
        this->next = node;
    else
        n->setNext(node);
}

void Ast::genCode(Unit* unit)
{
    IRBuilder* builder = new IRBuilder(unit);
    Node::setIRBuilder(builder);
    root->genCode();
}

void FuncDefStmt::genCode()
{
    Unit* unit = builder->getUnit();
    Function* func = new Function(unit, se);
    BasicBlock* entry = func->getEntry();
    // set the insert point to the entry basicblock of this function.

    builder->setInsertBB(entry);
    if (decl)
        decl->genCode();
    if (stmt)
        stmt->genCode();

    /**
     * Construct control flow graph. You need do set successors and predecessors
     * for each basic block. Todo
     */
    for (auto block = func->begin(); block != func->end(); block++)
    {
        //fprintf(stdout,"#################\n");
        Instruction* tmp = (*block)->begin();
        Instruction* last = (*block)->rbegin();

        while (tmp != last)
        {
            if (tmp->isCond() || tmp->isUncond())
                (*block)->remove(tmp);
            tmp = tmp->getNext();
        }
        //fprintf(stdout,"##################\n");

        if (last->isCond())
        {
            BasicBlock *truebranch, *falsebranch;
            truebranch = dynamic_cast<CondBrInstruction*>(last)->getTrueBranch();
            falsebranch = dynamic_cast<CondBrInstruction*>(last)->getFalseBranch();
             /*if(truebranch == nullptr)
                    fprintf(stdout,"1\n");
                if(falsebranch == nullptr)
                    fprintf(stdout,"2\n");*/

            if (truebranch->empty())
                new RetInstruction(nullptr, truebranch);
            else if (falsebranch->empty())
                new RetInstruction(nullptr, falsebranch);
            //fprintf(stdout,"@@@@@@@@@@@@@@@@@@@\n");
            (*block)->addSucc(truebranch);
            (*block)->addSucc(falsebranch);
            //fprintf(stdout,"@@@@@@@@@@@@@@@@@@@\n");
            truebranch->addPred(*block);
            falsebranch->addPred(*block);
            //fprintf(stdout,"@@@@@@@@@@@@@@@@@@@\n");
        }
        else if (last->isUncond())
        {
            //fprintf(stdout,"2\n");
            BasicBlock* dst = dynamic_cast<UncondBrInstruction*>(last)->getBranch();
            (*block)->addSucc(dst);
            dst->addPred(*block);
            //fprintf(stdout,"##############\n");
            if (dst->empty())
            {
                if (((FunctionType*)(se->getType()))->getRetType() == TypeSystem::intType)
                    new RetInstruction(new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0)),dst);
                else if (((FunctionType*)(se->getType()))->getRetType() == TypeSystem::voidType)
                    new RetInstruction(nullptr, dst);
            }
            //fprintf(stdout,"##################\n");
        }
        else if (!last->isRet())
        {
            //fprintf(stdout,"1\n");
            if (((FunctionType*)(se->getType()))->getRetType() == TypeSystem::voidType)
                new RetInstruction(nullptr, *block);
            //fprintf(stdout,"2\n");
        }
        //fprintf(stdout,"##################\n");
    }
}

BinaryExpr::BinaryExpr(SymbolEntry* se, int op, ExprNode* expr1, ExprNode* expr2) : ExprNode(se), op(op), expr1(expr1), expr2(expr2) 
{
    dst = new Operand(se);
    std::string op_str;
    switch (op)
    {
        case ADD:
            op_str = "+";
            break;
        case SUB:
            op_str = "-";
            break;
        case MUL:
            op_str = "*";
            break;
        case DIV:
            op_str = "/";
            break;
        case MOD:
            op_str = "%";
            break;
        case AND:
            op_str = "&&";
            break;
        case OR:
            op_str = "||";
            break;
        case LESS:
            op_str = "<";
            break;
        case LESSEQUAL:
            op_str = "<=";
            break;
        case GREATER:
            op_str = ">";
            break;
        case GREATEREQUAL:
            op_str = ">=";
            break;
        case EQU:
            op_str = "==";
            break;
        case NEQ:
            op_str = "!=";
            break;
    }
    if (expr1->getType()->isVoid() || expr2->getType()->isVoid())
        fprintf(stderr, "invalid operand of type \'void\' to binary \'opeartor%s\'\n",op_str.c_str());
    if (op >= BinaryExpr::AND && op <= BinaryExpr::NEQ)
    {
        //fprintf(stdout,"###############\n");
        type = TypeSystem::boolType;
        if (op == BinaryExpr::AND || op == BinaryExpr::OR)
        {
            if (expr1->getType()->isInt() && expr1->getType()->getSize() == INTSIZE)
            {
                //fprintf(stdout,"@@@@@@@@@@@@\n");
                int2boolExpr* temp = new int2boolExpr(expr1);
                this->expr1 = temp;
            }
            if (expr2->getType()->isInt() && expr2->getType()->getSize() == INTSIZE)
            {
                //fprintf(stdout,"###################\n");
                int2boolExpr* temp = new int2boolExpr(expr2);
                this->expr2 = temp;
            }
        }
    } 
    else
        type = TypeSystem::intType;
};

void BinaryExpr::genCode()
{
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    if (op == AND)
    {
        BasicBlock* trueBB = new BasicBlock(func);  // if the result of lhs is true, jump to the trueBB.
        expr1->genCode();
        backPatch(expr1->trueList(), trueBB);
        builder->setInsertBB(trueBB);  // set the insert point to the trueBB so that intructions
         // generated by expr2 will be inserted into it.
        expr2->genCode();
        true_list = expr2->trueList();
        false_list = merge(expr1->falseList(), expr2->falseList());
    }
    else if (op == OR)
    {
        //fprintf(stdout,"1\n");
        BasicBlock* trueBB = new BasicBlock(func);
        expr1->genCode();
        backPatch(expr1->falseList(), trueBB);
        builder->setInsertBB(trueBB);
        expr2->genCode();
        true_list = merge(expr1->trueList(), expr2->trueList());
        false_list = expr2->falseList();
        //fprintf(stdout,"2\n");
    }
    else if (op >= LESS && op <= NEQ)
    {
        expr1->genCode();
        expr2->genCode();
        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();
        if (src1->getType()->getSize() == 1)
        {
            Operand* dst = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
            new ZextInstruction(dst, src1, bb);
            src1 = dst;
        }
        if (src2->getType()->getSize() == 1)
        {
            Operand* dst = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
            new ZextInstruction(dst, src2, bb);
            src2 = dst;
        }
        int cmpopcode;
        switch (op)
        {
        case LESS:
            cmpopcode = CmpInstruction::L;
            break;
        case LESSEQUAL:
            cmpopcode = CmpInstruction::LE;
            break;
        case GREATER:
            cmpopcode = CmpInstruction::G;
            break;
        case GREATEREQUAL:
            cmpopcode = CmpInstruction::GE;
            break;
        case EQU:
            cmpopcode = CmpInstruction::E;
            break;
        case NEQ:
            cmpopcode = CmpInstruction::NE;
            break;
        }
        new CmpInstruction(cmpopcode, dst, src1, src2, bb);
        BasicBlock *truebb, *falsebb, *tempbb;
        truebb = new BasicBlock(func);
        falsebb = new BasicBlock(func);
        tempbb = new BasicBlock(func);

        true_list.push_back(new CondBrInstruction(truebb, tempbb, dst, bb));
        false_list.push_back(new UncondBrInstruction(falsebb, tempbb));
    }
    else if (op >= ADD && op <= MOD)
    {
        expr1->genCode();
        expr2->genCode();
        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();
        int opcode;
        switch (op)
        {
            case ADD:
                opcode = BinaryInstruction::ADD;
                break;
            case SUB:
                opcode = BinaryInstruction::SUB;
                break;
            case MUL:
                opcode = BinaryInstruction::MUL;
                break;
            case DIV:
                opcode = BinaryInstruction::DIV;
                break;
            case MOD:
                opcode = BinaryInstruction::MOD;
                break;
        }
        new BinaryInstruction(opcode, dst, src1, src2, bb);
    }
}

void Constant::genCode()
{
    // we don't need to generate code.
}

void Id::genCode()
{
    //fprintf(stdout,"id gencode\n");
    BasicBlock* bb = builder->getInsertBB();
    Operand* addr = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getAddr();
    if (type->isInt())
        new LoadInstruction(dst, addr, bb);
    else if (type->isArray())
    {
        if (arrayIdx)
        {
            Type* ele_type = ((ArrayType*)(this->type))->getElementType();
            Type* tmp_type = this->type;
            Operand* tempSrc = addr;
            Operand* tempDst = dst;
            ExprNode* curIdx = arrayIdx;

            bool pointer = false;
            bool first_dim = true;
            bool array_in_func = false;

            while (true)
            {
                if (((ArrayType*)tmp_type)->getLength() == -1)// 12/9 函数中使用数组。
                {
                    //fprintf(stdout,"###############\n");
                    Operand* dst1 = new Operand(new TemporarySymbolEntry(new PointerType(ele_type), SymbolTable::getLabel()));
                    tempSrc = dst1;
                    new LoadInstruction(dst1, addr, bb);
                    array_in_func = true;
                    first_dim = false;
                }
                if (!curIdx)// 12/15 数组维度没用完。
                {
                    //fprintf(stdout,"################\n");
                    Operand* dst1 = new Operand(new TemporarySymbolEntry(new PointerType(ele_type), SymbolTable::getLabel()));
                    Operand* idx = new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0));
                    new PtrInstruction(dst1, tempSrc, idx, bb);
                    tempDst = dst1;
                    pointer = true;
                    break;
                }

                //fprintf(stdout,"%d\n",idx->evaluate());
                curIdx->genCode();
                auto ptr_inst = new PtrInstruction(tempDst, tempSrc, curIdx->getOperand(), bb, array_in_func);
                if (!array_in_func && first_dim)
                {
                    ptr_inst->setFirst();
                    first_dim = false;
                }
                if (array_in_func)//数组作为参数使用时，空白的第一维度之后的部分可以像普通数组一样处理。
                    array_in_func = false;
                if (ele_type == TypeSystem::intType || ele_type == TypeSystem::constIntType)
                    break;
                //到下一维度。
                ele_type = ((ArrayType*)ele_type)->getElementType();
                tmp_type = ((ArrayType*)tmp_type)->getElementType();
                tempSrc = tempDst;//之前的目的作为之后的源。
                tempDst = new Operand(new TemporarySymbolEntry(new PointerType(ele_type), SymbolTable::getLabel()));
                curIdx = (ExprNode*)(curIdx->getNext());
            }
            dst = tempDst;
            // 12/1 右值
            if (!left && !pointer)
            {
                //fprintf(stdout,"#############\n");
                Operand* dst1 = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
                new LoadInstruction(dst1, dst, bb);
                dst = dst1;
            }

        }
        else// 不带[]
        {
            //fprintf(stdout,"####################\n");
            if (((ArrayType*)(this->type))->getLength() == -1)//参数作为函数内部调用函数的参数。
            {
                //fprintf(stdout,"################\n");
                Operand* dst1 = new Operand(new TemporarySymbolEntry(new PointerType(((ArrayType*)(this->type))->getElementType()), SymbolTable::getLabel()));
                new LoadInstruction(dst1, addr, bb);
                dst = dst1;
            }
            else// 12/1 数组ID作为参数。
            {
                //fprintf(stdout,"@@@@@@@@@@@@@@@\n");
                Operand* idx = new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0));
                auto tmp_ptr = new PtrInstruction(dst, addr, idx, bb);
                tmp_ptr->setFirst();
            }
        }
    }
}

void IfStmt::genCode()
{
    Function* func;
    BasicBlock *then_bb, *end_bb;

    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    cond->genCode();

    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), end_bb);

    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(end_bb);
}

void IfElseStmt::genCode()
{
    Function* func;
    BasicBlock *then_bb, *else_bb, *end_bb;
    // bb = builder->getInsertBB();
    func = builder->getInsertBB()->getParent();
    then_bb = new BasicBlock(func);
    else_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    cond->genCode();
    // Operand* IfElsecond = cond->getOperand();
    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), else_bb);

    // new CondBrInstruction(then_bb,else_bb,IfElsecond,bb);

    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(else_bb);
    elseStmt->genCode();
    else_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, else_bb);

    builder->setInsertBB(end_bb);
}

void CompoundStmt::genCode()
{
    if (stmt)
        stmt->genCode();
}

void StmtsNode::genCode()
{
    stmt1->genCode();
    stmt2->genCode();
}

void DeclStmt::genCode()
{
    //fprintf(stdout,"###############\n");
    IdentifierSymbolEntry* se = dynamic_cast<IdentifierSymbolEntry*>(id->getSymbolEntry());
    if (se->isGlobal())
    {
        Operand* addr;
        SymbolEntry* addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
        unit.insertGlobal(se);
        mUnit.insertGlobal(se);
    }
    else if (se->isLocal() || se->isParam())
    {
        Function* func = builder->getInsertBB()->getParent();
        BasicBlock* entry = func->getEntry();
        Instruction* alloca;
        Operand* addr;
        SymbolEntry* addr_se;
        Type* type;
        type = new PointerType(se->getType());
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);
        alloca = new AllocaInstruction(addr, se);
        entry->insertFront(alloca);
        Operand* temp;
        if (se->isParam())
            temp = se->getAddr();
        se->setAddr(addr);
        if (expr)      // 12/5 如果为参数，则不可能有expr，只有局部变量才有
        {
            if (expr->isArrayInit())
            {
                //fprintf(stdout,"##############\n");
                Operand* init = nullptr;
                BasicBlock* bb = builder->getInsertBB();
                ExprNode* temp = expr;
                std::stack<ExprNode*> stack;
                std::vector<int> idx;
                idx.push_back(0);
                while (temp)
                {
                    if (temp->isArrayInit())
                    {
                        //fprintf(stdout,"##############\n");
                        stack.push(temp);
                        idx.push_back(0);
                        temp = ((ArrayInit*)temp)->getExpr();
                        continue;
                    }
                    else
                    {
                        //fprintf(stdout,"@@@@@@@@@@@@\n");
                        temp->genCode();
                        Type* type = ((ArrayType*)(se->getType()))->getElementType();
                        Operand* tempSrc = addr;
                        Operand* tempDst;
                        Operand* index;
                        bool first = true;
                        int i = 1;

                        while (true)
                        {
                            //fprintf(stdout,"###############\n");
                            tempDst = new Operand(new TemporarySymbolEntry(new PointerType(type),SymbolTable::getLabel()));
                            index = (new Constant(new ConstantSymbolEntry(TypeSystem::intType, idx[i++])))->getOperand();
                            auto getp = new PtrInstruction(tempDst, tempSrc, index, bb);
                            getp->setInit(init);
                            if (first)
                            {
                                getp->setFirst();
                                first = false;
                            }
                            if (type == TypeSystem::intType || type == TypeSystem::constIntType)
                            {
                                getp->setLast();
                                init = tempDst;
                                break;
                            }
                            type = ((ArrayType*)type)->getElementType();
                            tempSrc = tempDst;//目的作为下一次的源。
                        }
                        new StoreInstruction(tempDst, temp->getOperand(), bb);
                    }

                    while (true)
                    {
                        if (temp->getNext())//这一维度还有赋值。
                        {
                            //fprintf(stdout,"@@@@@@@@@@@@@@@\n");
                            temp = (ExprNode*)(temp->getNext());
                            idx[idx.size() - 1]++;
                            //fprintf(stdout,"%d\n",idx[idx.size() - 1]);
                            break;
                        }
                        else
                        {
                            //回上一维度。
                            //fprintf(stdout,"###############\n");
                            temp = stack.top();
                            stack.pop();
                            idx.pop_back();
                            if (stack.empty())
                                break;
                        }
                    }
                    if (stack.empty())
                        break;
                }
            }
            else
            {
                BasicBlock* bb = builder->getInsertBB();
                expr->genCode();
                Operand* src = expr->getOperand();
                new StoreInstruction(addr, src, bb);
            }
        }
        if (se->isParam())
        {
            BasicBlock* bb = builder->getInsertBB();
            new StoreInstruction(addr, temp, bb);
        }
    }
    if (this->getNext())
        this->getNext()->genCode();
}

void ReturnStmt::genCode()
{
    BasicBlock* bb = builder->getInsertBB();
    Operand* src = nullptr;
    if (retValue)
    {
        retValue->genCode();
        src = retValue->getOperand();
    }
    new RetInstruction(src, bb);
}

void ExprStmt::genCode()
{
    expr->genCode();
}

void ContinueStmt::genCode()
{
    Function* func = builder->getInsertBB()->getParent();
    BasicBlock* bb = builder->getInsertBB();
    new UncondBrInstruction(((WhileStmt*)whileStmt)->get_cond_bb(), bb);
    BasicBlock* continue_next_bb = new BasicBlock(func);
    builder->setInsertBB(continue_next_bb);
}

void BreakStmt::genCode()
{
    Function* func = builder->getInsertBB()->getParent();
    BasicBlock* bb = builder->getInsertBB();
    new UncondBrInstruction(((WhileStmt*)whileStmt)->get_end_bb(), bb);
    BasicBlock* break_next_bb = new BasicBlock(func);
    builder->setInsertBB(break_next_bb);
}

void WhileStmt::genCode()
{
    Function* func;
    BasicBlock *cond_bb, *while_bb, *end_bb, *bb;
    bb = builder->getInsertBB();
    func = builder->getInsertBB()->getParent();
    cond_bb = new BasicBlock(func);
    while_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    this->cond_bb = cond_bb;
    this->end_bb = end_bb;

    new UncondBrInstruction(cond_bb, bb);

    builder->setInsertBB(cond_bb);
    cond->genCode();
    backPatch(cond->trueList(), while_bb);
    backPatch(cond->falseList(), end_bb);
    // Operand* condoperand= cond->getOperand();
    // new CondBrInstruction(while_bb,end_bb,condoperand,cond_bb);

    builder->setInsertBB(while_bb);
    stmt->genCode();

    while_bb = builder->getInsertBB();
    new UncondBrInstruction(cond_bb, while_bb);

    builder->setInsertBB(end_bb);
}

void BlankStmt::genCode()
{
    
}

void ArrayInit::genCode()
{

}

void FuncCallExpr::genCode()
{
    std::vector<Operand*> operands;
    ExprNode* temp = param;
    while (temp)
    {
        temp->genCode();
        operands.push_back(temp->getOperand());
        temp = ((ExprNode*)temp->getNext());
    }
    BasicBlock* bb = builder->getInsertBB();
    new CallInstruction(dst, symbolEntry, operands, bb);
}

void UnaryExpr::genCode()
{
    
    expr->genCode();
    if (op == NOT)
    {
        BasicBlock* bb = builder->getInsertBB();
        Operand* src = expr->getOperand();
        if (expr->getType()->getSize() == 32) {
            Operand* temp = new Operand(new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel()));
            new CmpInstruction(CmpInstruction::NE, temp, src,new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0)),bb);
            src = temp;
        }
        new NotInstruction(dst, src, bb);
    }
    else if (op == SUB)
    {
        Operand* src2;
        BasicBlock* bb = builder->getInsertBB();
        Operand* src1 = new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0));
        if (expr->getType()->getSize() == 1)
        {
            src2 = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
            new ZextInstruction(src2, expr->getOperand(), bb);
        }
        else
            src2 = expr->getOperand();
        new BinaryInstruction(BinaryInstruction::SUB, dst, src1, src2, bb);
    }
}
void ExprNode::genCode()
{
    
}

bool WhileStmt::typeCheck(Type* retType)
{
    if (stmt)
        return stmt->typeCheck(retType);
    return false;
}

bool ExprStmt::typeCheck(Type* retType)
{
    return false;
}

void AssignStmt::genCode()
{
    BasicBlock* bb = builder->getInsertBB();
    expr->genCode();
    Operand* addr;

    if (lval->getOriginType()->isInt())
        addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymbolEntry())->getAddr();
    else if (lval->getOriginType()->isArray())
    {
        ((Id*)lval)->setLeft();
        lval->genCode();
        addr = lval->getOperand();

    }
    Operand* src = expr->getOperand();
    /***
     * We haven't implemented array yet, the lval can only be ID. So we just
     * store the result of the `expr` to the addr of the id. If you want to
     * implement array, you have to caculate the address first and then store
     * the result into it.
     */
    new StoreInstruction(addr, src, bb);
}

void int2boolExpr::genCode()
{
    expr->genCode();
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    BasicBlock* trueBB = new BasicBlock(func);
    BasicBlock* tempbb = new BasicBlock(func);
    BasicBlock* falseBB = new BasicBlock(func);

    new CmpInstruction(CmpInstruction::NE, this->dst, this->expr->getOperand(), new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0)), bb);
    this->trueList().push_back(new CondBrInstruction(trueBB, tempbb, this->dst, bb));
    this->falseList().push_back(new UncondBrInstruction(falseBB, tempbb));
}

bool Ast::typeCheck(Type* retType)
{
    if (root != nullptr)
        return root->typeCheck();
    return false;
}

bool FuncDefStmt::typeCheck(Type* retType)
{
    SymbolEntry* se = this->getSymbolEntry();
    Type* ret = ((FunctionType*)(se->getType()))->getRetType();
    StmtNode* stmt = this->stmt;
    if (stmt == nullptr)
    {
        if (ret != TypeSystem::voidType)
            fprintf(stderr, "<Return error>\n");//12.18
        return false;
    }
    if (!stmt->typeCheck(ret))
    {
        fprintf(stderr, "<No return statement>\n");//12.18
        return false;
    }
    return false;
}

bool IfStmt::typeCheck(Type* retType)
{
    if (thenStmt)
        return thenStmt->typeCheck(retType);
    return false;
}

bool IfElseStmt::typeCheck(Type* retType)
{
    bool flag1 = false, flag2 = false;
    if (thenStmt)
        flag1 = thenStmt->typeCheck(retType);
    if (elseStmt)
        flag2 = elseStmt->typeCheck(retType);
    return flag1 || flag2;
}

bool CompoundStmt::typeCheck(Type* retType)
{
    if (stmt)
        return stmt->typeCheck(retType);
    return false;
}

bool StmtsNode::typeCheck(Type* retType)
{
    bool flag1 = false, flag2 = false;
    if (stmt1)
        flag1 = stmt1->typeCheck(retType);
    if (stmt2)
        flag2 = stmt2->typeCheck(retType);
    return flag1 || flag2;
}

FuncCallExpr::FuncCallExpr(SymbolEntry* se, ExprNode* param) : ExprNode(se), param(param)
{
    dst = nullptr;
    SymbolEntry* tmp_se = se;
    int paramCnt = 0;
    ExprNode* temp = param;
    while (temp)
    {
        paramCnt++;
        temp = (ExprNode*)(temp->getNext());
    }
    while (tmp_se)
    {
        Type* type = tmp_se->getType();
        std::vector<Type*> params = ((FunctionType*)type)->getParamsType();
        if ((long unsigned int)paramCnt == params.size())
        {
            this->symbolEntry = tmp_se;
            break;
        }
        tmp_se = tmp_se->getNext();
    }
    if (symbolEntry)
    {
        Type* type = symbolEntry->getType();
        this->type = ((FunctionType*)type)->getRetType();
        if (this->type != TypeSystem::voidType)
        {
            SymbolEntry* se = new TemporarySymbolEntry(this->type, SymbolTable::getLabel());
            dst = new Operand(se);
        }
        std::vector<Type*> params = ((FunctionType*)type)->getParamsType();
        ExprNode* temp = param;
        for (auto it = params.begin(); it != params.end(); it++)
        {
            if (temp == nullptr)
            {
                fprintf(stderr, "<Too few arguments to function %s %s>\n", symbolEntry->toStr().c_str(), type->toStr().c_str());
                break;
            }
            else if ((*it)->getKind() != temp->getType()->getKind())
                fprintf(stderr, "<Parameter's type %s can't convert to %s>\n",temp->getType()->toStr().c_str(),(*it)->toStr().c_str());
            temp = (ExprNode*)(temp->getNext());
        }
        if (temp != nullptr)
        {
            fprintf(stderr, "<Too many arguments to function %s %s>\n",symbolEntry->toStr().c_str(), type->toStr().c_str());
        }
    }
    if (((IdentifierSymbolEntry*)se)->isSysy())
    {
        unit.insertDeclare(se);
    }
}

AssignStmt::AssignStmt(ExprNode* lval, ExprNode* expr) : lval(lval), expr(expr)
{
    Type* type = ((Id*)lval)->getType();
    SymbolEntry* se = lval->getSymbolEntry();
    bool flag = true;
    if (type->isInt())
    {
        if (((IntType*)type)->isConst())
        {
            fprintf(stderr, "<Cannot assign to variable \'%s\' with const-qualified type \'%s\'>\n",
                    ((IdentifierSymbolEntry*)se)->toStr().c_str(), type->toStr().c_str());
            flag = false;
        }
    }
    else if (type->isArray())
    {
        fprintf(stderr, "<Array type \'%s\' is not assignable>\n", type->toStr().c_str());
        flag = false;
    }
    if (flag && !expr->getType()->isInt())
    {
        fprintf(stderr,"<Cannot initialize a variable of type \'int\' with an rvalue of type \'%s\'>\n",expr->getType()->toStr().c_str());
    }
}

Type* Id::getType()
{
    SymbolEntry* se = this->getSymbolEntry();
    if (!se)
        return TypeSystem::voidType;
    Type* type = se->getType();
    if (!arrayIdx)
        return type;
    else if (!type->isArray())
    {
        fprintf(stderr, "<Subscripted value is not an array>\n");
        return TypeSystem::voidType;
    }
    else
    {
        ArrayType* temp1 = (ArrayType*)type;
        ExprNode* temp2 = arrayIdx;
        while (!temp1->getElementType()->isInt())
        {
            if (!temp2)
                return temp1;
            temp2 = (ExprNode*)(temp2->getNext());
            temp1 = (ArrayType*)(temp1->getElementType());
        }
        if (!temp2)
        {
            fprintf(stderr, "<Subscripted value is not an array>\n");
            return temp1;
        }
        else if (temp2->getNext())
        {
            fprintf(stderr, "<Subscripted value is not an array>\n");
            return TypeSystem::voidType;
        }
    }
    return TypeSystem::intType;
}

void ExprNode::output(int level)
{
    std::string name, type;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    fprintf(yyout, "%*cconst string\ttype:%s\t%s\n", level, ' ', type.c_str(),
            name.c_str());
}

void Ast::output()
{
    fprintf(yyout, "program\n");
    if (root != nullptr)
        root->output(4);
}

void BinaryExpr::output(int level)
{
    std::string op_str;
    switch (op) {
        case ADD:
            op_str = "add";
            break;
        case SUB:
            op_str = "sub";
            break;
        case MUL:
            op_str = "mul";
            break;
        case DIV:
            op_str = "div";
            break;
        case MOD:
            op_str = "mod";
            break;
        case AND:
            op_str = "and";
            break;
        case OR:
            op_str = "or";
            break;
        case LESS:
            op_str = "less";
            break;
        case LESSEQUAL:
            op_str = "lessequal";
            break;
        case GREATER:
            op_str = "greater";
            break;
        case GREATEREQUAL:
            op_str = "greaterequal";
            break;
        case EQU:
            op_str = "equal";
            break;
        case NEQ:
            op_str = "notequal";
            break;
    }
    fprintf(yyout, "%*cBinaryExpr\top: %s\ttype: %s\n", level, ' ', op_str.c_str(), type->toStr().c_str());
    expr1->output(level + 4);
    expr2->output(level + 4);
}

int BinaryExpr::evaluate()
{
    int value=0;
    switch (op)
    {
        case ADD:
            value = expr1->evaluate() + expr2->evaluate();
            break;
        case SUB:
            value = expr1->evaluate() - expr2->evaluate();
            break;
        case MUL:
            value = expr1->evaluate() * expr2->evaluate();
            break;
        case DIV:
            value = expr1->evaluate() / expr2->evaluate();
            break;
        case MOD:
            value = expr1->evaluate() % expr2->evaluate();
            break;
        case AND:
            value = expr1->evaluate() && expr2->evaluate();
            break;
        case OR:
            value = expr1->evaluate() || expr2->evaluate();
            break;
        case LESS:
            value = expr1->evaluate() < expr2->evaluate();
            break;
        case LESSEQUAL:
            value = expr1->evaluate() <= expr2->evaluate();
            break;
        case GREATER:
            value = expr1->evaluate() > expr2->evaluate();
            break;
        case GREATEREQUAL:
            value = expr1->evaluate() >= expr2->evaluate();
            break;
        case EQU:
            value = expr1->evaluate() == expr2->evaluate();
            break;
        case NEQ:
            value = expr1->evaluate() != expr2->evaluate();
            break;
    }
    return value;
}

UnaryExpr::UnaryExpr(SymbolEntry* se, int op, ExprNode* expr) : ExprNode(se, UNARYEXPR), op(op), expr(expr)
{
    std::string op_str = op == UnaryExpr::NOT ? "!" : "-";
    if (expr->getType()->isVoid())
        fprintf(stderr, "invalid operand of type \'void\' to unary \'opeartor%s\'\n", op_str.c_str());
    if (op == UnaryExpr::NOT)
    {
        type = TypeSystem::intType;
        dst = new Operand(se);
        if (expr->isUnaryExpr())
        {
            UnaryExpr* ue = (UnaryExpr*)expr;
            if (ue->getOp() == UnaryExpr::NOT)
            {
                if (ue->getType() == TypeSystem::intType)
                    ue->setType(TypeSystem::boolType);
                // type = TypeSystem::intType;
            }
        }
    }
    else if (op == UnaryExpr::SUB)
    {
        type = TypeSystem::intType;
        dst = new Operand(se);
        if (expr->isUnaryExpr())
        {
            UnaryExpr* ue = (UnaryExpr*)expr;
            if (ue->getOp() == UnaryExpr::NOT)
                if (ue->getType() == TypeSystem::intType)
                    ue->setType(TypeSystem::boolType);
        }
    }
};

void UnaryExpr::output(int level)
{
    std::string op_str;
    switch (op)
    {
        case NOT:
            op_str = "not";
            break;
        case SUB:
            op_str = "minus";
            break;
    }
    fprintf(yyout, "%*cUnaryExpr\top: %s\ttype: %s\n", level, ' ', op_str.c_str(), type->toStr().c_str());
    expr->output(level + 4);
}

int UnaryExpr::evaluate()
{
    int value;
    switch (op)
    {
        case NOT:
            value = !(expr->evaluate());
            break;
        case SUB:
            value = -(expr->evaluate());
            break;
    }
    return value;
}

void FuncCallExpr::output(int level) {
    std::string name, type;
    int scope;
    if (symbolEntry) {
        name = symbolEntry->toStr();
        type = symbolEntry->getType()->toStr();
        scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
        fprintf(yyout, "%*cFuncCallExpr\tfunction name: %s\tscope: %d\ttype: %s\n",
                level, ' ', name.c_str(), scope, type.c_str());
        Node* temp = param;
        while (temp) {
            temp->output(level + 4);
            temp = temp->getNext();
        }
    }
}

void Constant::output(int level) {
    std::string type, value;
    type = symbolEntry->getType()->toStr();
    value = symbolEntry->toStr();
    fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
}

int Constant::evaluate()
{
    // assert(symbolEntry->getType()->isInt());
    return ((ConstantSymbolEntry*)symbolEntry)->getValue();
}

int Id::evaluate()
{
    // assert(symbolEntry->getType()->isInt());
    return ((IdentifierSymbolEntry*)symbolEntry)->getValue();
}

void Id::output(int level)
{
    std::string name, type;
    int scope;
    if (symbolEntry)
    {
        name = symbolEntry->toStr();
        type = symbolEntry->getType()->toStr();
        scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
        fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ', name.c_str(), scope, type.c_str());
        if (arrayIdx)
        {
            ExprNode* temp = arrayIdx;
            int i = 0;
            while (temp)
            {
                temp->output(level + 4 + 4 * i++);
                temp = (ExprNode*)(temp->getNext());
            }
        }
    }
}

void ArrayInit::output(int level)
{
    std::string type;
    if (symbolEntry->getType())
        type = symbolEntry->getType()->toStr();
    fprintf(yyout, "%*cInitValueListExpr\ttype: %s\n", level, ' ',
            type.c_str());
    Node* temp = expr;
    while (temp)
    {
        temp->output(level + 4);
        temp = temp->getNext();
    }
}

void ArrayInit::addExpr(ExprNode* expr)
{
    if (this->expr == nullptr)
    {
        assert(childCnt == 0);
        childCnt++;
        this->expr = expr;
    }
    else
    {
        childCnt++;
        this->expr->setNext(expr);//12.18 下一个维度。
    }
    //fprintf(stdout,"%d\n",childCnt);
}

bool ArrayInit::isFull()
{
    ArrayType* type = (ArrayType*)(this->symbolEntry->getType());
    return childCnt == type->getLength();
}

void ArrayInit::fill()
{
    Type* type = ((ArrayType*)(this->getType()))->getElementType();
    if (type->isArray())
    {
        while (!isFull())
            this->addExpr(new ArrayInit(new ConstantSymbolEntry(type)));
        ExprNode* temp = expr;
        while (temp)
        {
            ((ArrayInit*)temp)->fill();
            temp = (ExprNode*)(temp->getNext());
        }
    }
    if (type->isInt())
    {
        while (!isFull())
            this->addExpr(new Constant(new ConstantSymbolEntry(type, 0)));
        return;
    }
}

void int2boolExpr::output(int level)
{
    fprintf(yyout, "%*cImplictCastExpr\ttype: %s to %s\n", level, ' ', expr->getType()->toStr().c_str(), type->toStr().c_str());
    this->expr->output(level + 4);
}

void CompoundStmt::output(int level)
{
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    if (stmt)
        stmt->output(level + 4);
}

void StmtsNode::output(int level)
{
    // fprintf(yyout, "%*cSequence\n", level, ' ');
    stmt1->output(level);
    stmt2->output(level);
}

void DeclStmt::output(int level)
{
    fprintf(yyout, "%*cDeclStmt\n", level, ' ');
    id->output(level + 4);
    if (expr)
        expr->output(level + 4);
    if (this->getNext())
        this->getNext()->output(level);
}

void BlankStmt::output(int level)
{
    fprintf(yyout, "%*cBlankStmt\n", level, ' ');
}

void IfStmt::output(int level)
{
    fprintf(yyout, "%*cIfStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
}

void IfElseStmt::output(int level)
{
    fprintf(yyout, "%*cIfElseStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
    elseStmt->output(level + 4);
}

void WhileStmt::output(int level)
{
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    cond->output(level + 4);
    stmt->output(level + 4);
}
void BreakStmt::output(int level)
{
    fprintf(yyout, "%*cBreakStmt\n", level, ' ');
}

void ContinueStmt::output(int level)
{
    fprintf(yyout, "%*cContinueStmt\n", level, ' ');
}

void ReturnStmt::output(int level)
{
    fprintf(yyout, "%*cReturnStmt\n", level, ' ');
    if (retValue != nullptr)
        retValue->output(level + 4);
}

void AssignStmt::output(int level)
{
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);
    expr->output(level + 4);
}

void ExprStmt::output(int level)
{
    fprintf(yyout, "%*cExprStmt\n", level, ' ');
    expr->output(level + 4);
}

void FuncDefStmt::output(int level)
{
    std::string name, type;
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%*cFuncDefStmtine\tfunction name: %s\ttype: %s\n", level, ' ', name.c_str(), type.c_str());
    if (decl)
        decl->output(level + 4);
    stmt->output(level + 4);
}