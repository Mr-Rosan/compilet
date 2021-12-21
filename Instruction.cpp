#include "Instruction.h"
#include <iostream>
#include <sstream>
#include "BasicBlock.h"
#include "Function.h"
#include "Type.h"
extern FILE* yyout;

Instruction::Instruction(unsigned instType, BasicBlock* insert_bb)
{
    prev = next = this;
    opcode = -1;
    this->instType = instType;
    if (insert_bb != nullptr)
    {
        insert_bb->insertBack(this);
        parent = insert_bb;
    }
}

Instruction::~Instruction()
{
    parent->remove(this);
}

BasicBlock* Instruction::getParent()
{
    return parent;
}

void Instruction::setParent(BasicBlock* bb)
{
    parent = bb;
}

void Instruction::setNext(Instruction* inst)
{
    next = inst;
}

void Instruction::setPrev(Instruction* inst)
{
    prev = inst;
}

Instruction* Instruction::getNext()
{
    return next;
}

Instruction* Instruction::getPrev()
{
    return prev;
}

BinaryInstruction::BinaryInstruction(unsigned opcode, Operand* dst, Operand* src1, Operand* src2, BasicBlock* insert_bb) : Instruction(BINARY, insert_bb)
{
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src1);
    operands.push_back(src2);
    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
}

BinaryInstruction::~BinaryInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void BinaryInstruction::output() const
{
    std::string s1, s2, s3, op, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    s3 = operands[2]->toStr();
    type = operands[0]->getType()->toStr();
    switch (opcode)
    {
        case ADD:
            op = "add";
            break;
        case SUB:
            op = "sub";
            break;
        case MUL:
            op = "mul";
            break;
        case DIV:
            op = "sdiv";
            break;
        case MOD:
            op = "srem";
            break;
        default:
            break;
    }
    fprintf(yyout, "  %s = %s %s %s, %s\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str(), s3.c_str());
}

CmpInstruction::CmpInstruction(unsigned opcode, Operand* dst, Operand* src1, Operand* src2, BasicBlock* insert_bb) : Instruction(CMP, insert_bb)
{
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src1);
    operands.push_back(src2);
    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
}

CmpInstruction::~CmpInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void CmpInstruction::output() const
{
    std::string s1, s2, s3, op, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    s3 = operands[2]->toStr();
    type = operands[1]->getType()->toStr();
    switch (opcode)
    {
    case E:
        op = "eq";
        break;
    case NE:
        op = "ne";
        break;
    case L:
        op = "slt";
        break;
    case LE:
        op = "sle";
        break;
    case G:
        op = "sgt";
        break;
    case GE:
        op = "sge";
        break;
    default:
        op = "";
        break;
    }
    fprintf(yyout, "  %s = icmp %s %s %s, %s\n", s1.c_str(), op.c_str(), type.c_str(), s2.c_str(), s3.c_str());
}

UncondBrInstruction::UncondBrInstruction(BasicBlock* to, BasicBlock* insert_bb) : Instruction(UNCOND, insert_bb)
{
    branch = to;
}

void UncondBrInstruction::output() const
{
    fprintf(yyout, "  br label %%B%d\n", branch->getNo());
}

void UncondBrInstruction::setBranch(BasicBlock* bb)
{
    branch = bb;
}

BasicBlock* UncondBrInstruction::getBranch()
{
    return branch;
}

CondBrInstruction::CondBrInstruction(BasicBlock* true_branch, BasicBlock* false_branch, Operand* cond, BasicBlock* insert_bb) : Instruction(COND, insert_bb)
{
    this->true_branch = true_branch;
    this->false_branch = false_branch;
    cond->addUse(this);
    operands.push_back(cond);
}

CondBrInstruction::~CondBrInstruction()
{
    operands[0]->removeUse(this);
}

void CondBrInstruction::output() const
{
    std::string cond, type;
    cond = operands[0]->toStr();
    type = operands[0]->getType()->toStr();
    int true_label = true_branch->getNo();
    int false_label = false_branch->getNo();
    fprintf(yyout, "  br %s %s, label %%B%d, label %%B%d\n", type.c_str(), cond.c_str(), true_label, false_label);
}

void CondBrInstruction::setFalseBranch(BasicBlock* bb)
{
    false_branch = bb;
}

BasicBlock* CondBrInstruction::getFalseBranch()
{
    return false_branch;
}

void CondBrInstruction::setTrueBranch(BasicBlock* bb)
{
    true_branch = bb;
}

BasicBlock* CondBrInstruction::getTrueBranch()
{
    return true_branch;
}

RetInstruction::RetInstruction(Operand* src, BasicBlock* insert_bb) : Instruction(RET, insert_bb)
{
    if (src != nullptr)
    {
        operands.push_back(src);
        src->addUse(this);
    }
}

RetInstruction::~RetInstruction()
{
    if (!operands.empty())
        operands[0]->removeUse(this);
}

void RetInstruction::output() const
{
    if (operands.empty())
        fprintf(yyout, "  ret void\n");
    else
    {
        std::string ret, type;
        ret = operands[0]->toStr();
        type = operands[0]->getType()->toStr();
        fprintf(yyout, "  ret %s %s\n", type.c_str(), ret.c_str());
    }
}

AllocaInstruction::AllocaInstruction(Operand* dst, SymbolEntry* se, BasicBlock* insert_bb) : Instruction(ALLOCA, insert_bb)
{
    operands.push_back(dst);
    dst->setDef(this);
    this->se = se;
}

AllocaInstruction::~AllocaInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
}

void AllocaInstruction::output() const
{
    std::string dst, type;
    dst = operands[0]->toStr();
    if (se->getType()->isInt())
    {
        type = se->getType()->toStr();
        fprintf(yyout, "  %s = alloca %s, align 4\n", dst.c_str(), type.c_str());
    }
    else if (se->getType()->isArray())
    {
        type = se->getType()->toStr();
        // type = operands[0]->getSymbolEntry()->getType()->toStr();
        fprintf(yyout, "  %s = alloca %s, align 4\n", dst.c_str(), type.c_str());
    }
}

LoadInstruction::LoadInstruction(Operand* dst, Operand* src_addr, BasicBlock* insert_bb) : Instruction(LOAD, insert_bb)
{
    operands.push_back(dst);
    operands.push_back(src_addr);
    dst->setDef(this);
    src_addr->addUse(this);
}

LoadInstruction::~LoadInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

void LoadInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    std::string src_type;
    std::string dst_type;
    dst_type = operands[0]->getType()->toStr();
    src_type = operands[1]->getType()->toStr();
    fprintf(yyout, "  %s = load %s, %s %s, align 4\n", dst.c_str(), dst_type.c_str(), src_type.c_str(), src.c_str());
}

StoreInstruction::StoreInstruction(Operand* dst_addr, Operand* src, BasicBlock* insert_bb) : Instruction(STORE, insert_bb)
{
    operands.push_back(dst_addr);
    operands.push_back(src);
    dst_addr->addUse(this);
    src->addUse(this);
}

StoreInstruction::~StoreInstruction()
{
    operands[0]->removeUse(this);
    operands[1]->removeUse(this);
}

CallInstruction::CallInstruction(Operand* dst, SymbolEntry* func, std::vector<Operand*> params, BasicBlock* insert_bb) : Instruction(CALL, insert_bb), func(func), dst(dst)
{
    operands.push_back(dst);
    if (dst)
        dst->setDef(this);
    for (auto param : params)
    {
        operands.push_back(param);
        param->addUse(this);
    }
}

CallInstruction::~CallInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    for (long unsigned int i = 1; i < operands.size(); i++)
        operands[i]->removeUse(this);
}

ZextInstruction::ZextInstruction(Operand* dst, Operand* src, BasicBlock* insert_bb) : Instruction(ZEXT, insert_bb)
{
    operands.push_back(dst);
    operands.push_back(src);
    dst->setDef(this);
    src->addUse(this);
}

ZextInstruction::~ZextInstruction() 
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

NotInstruction::NotInstruction(Operand* dst, Operand* src, BasicBlock* insert_bb) : Instruction(XOR, insert_bb)
{
    operands.push_back(dst);
    operands.push_back(src);
    dst->setDef(this);
    src->addUse(this);
}

NotInstruction::~NotInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

PtrInstruction::PtrInstruction(Operand* dst, Operand* arr, Operand* idx, BasicBlock* insert_bb, bool paramFirst) : Instruction(GEP, insert_bb), paramFirst(paramFirst) 
{
    operands.push_back(dst);
    operands.push_back(arr);
    operands.push_back(idx);
    dst->setDef(this);
    arr->addUse(this);
    idx->addUse(this);
    firstDim = false;
    init = nullptr;
    last = false;
}

void StoreInstruction::output() const
{
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    std::string dst_type = operands[0]->getType()->toStr();
    std::string src_type = operands[1]->getType()->toStr();

    fprintf(yyout, "  store %s %s, %s %s, align 4\n", src_type.c_str(), src.c_str(), dst_type.c_str(), dst.c_str());
}

void PtrInstruction::output() const
{
    Operand* dst = operands[0];
    Operand* arr = operands[1];
    Operand* idx = operands[2];
    std::string arrType = arr->getType()->toStr();

    if (paramFirst)
        fprintf(yyout, "  %s = getelementptr inbounds %s, %s %s, i32 %s\n", dst->toStr().c_str(), arrType.substr(0, arrType.size() - 1).c_str(), arrType.c_str(),arr->toStr().c_str(), idx->toStr().c_str());
    else
        fprintf(yyout, "  %s = getelementptr inbounds %s, %s %s, i32 0, i32 %s\n",dst->toStr().c_str(), arrType.substr(0,  arrType.size() - 1).c_str(), arrType.c_str(), arr->toStr().c_str(), idx->toStr().c_str());

    // %t22 = getelementptr inbounds [2 x i32], [2 x i32]* %t1, i32 0, i32 1
}

void CallInstruction::output() const
{
    fprintf(yyout, "  ");
    if (operands[0])
        fprintf(yyout, "%s = ", operands[0]->toStr().c_str());
    FunctionType* type = (FunctionType*)(func->getType());
    fprintf(yyout, "call %s %s(", type->getRetType()->toStr().c_str(),
            func->toStr().c_str());
    for (long unsigned int i = 1; i < operands.size(); i++) {
        if (i != 1)
            fprintf(yyout, ", ");
        fprintf(yyout, "%s %s", operands[i]->getType()->toStr().c_str(), operands[i]->toStr().c_str());
    }
    fprintf(yyout, ")\n");
}

void ZextInstruction::output() const
{
    Operand* dst = operands[0];
    Operand* src = operands[1];
    fprintf(yyout, "  %s = zext %s %s to i32\n", dst->toStr().c_str(), src->getType()->toStr().c_str(), src->toStr().c_str());
}

void NotInstruction::output() const
{
    Operand* dst = operands[0];
    Operand* src = operands[1];
    fprintf(yyout, "  %s = xor %s %s, true\n", dst->toStr().c_str(), src->getType()->toStr().c_str(), src->toStr().c_str());
}

PtrInstruction::~PtrInstruction()
{
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

MachineOperand* Instruction::genMachineOperand(Operand* ope)
{
    auto se = ope->getEntry();
    MachineOperand* mope = nullptr;
    if (se->isConstant())
        mope = new MachineOperand(MachineOperand::IMM, dynamic_cast<ConstantSymbolEntry*>(se)->getValue());
    else if (se->isTemporary())
        mope = new MachineOperand(MachineOperand::VREG, dynamic_cast<TemporarySymbolEntry*>(se)->getLabel());
    else if (se->isVariable())
    {
        auto id_se = dynamic_cast<IdentifierSymbolEntry*>(se);
        if (id_se->isGlobal())
            mope = new MachineOperand(id_se->toStr().c_str());
        else if (id_se->isParam())
        {
            //fprintf(stdout,"%d\n",id_se->getValue());
            if (id_se->getParamNo() < 4)//参数个数限制。
                mope = new MachineOperand(MachineOperand::REG, id_se->getParamNo());
            else
                mope = new MachineOperand(MachineOperand::REG, 3);
        }
        else
            exit(0);
    }
    return mope;
}

MachineOperand* Instruction::genMachineReg(int reg)
{
    return new MachineOperand(MachineOperand::REG, reg);
}

MachineOperand* Instruction::genMachineVReg()
{
    return new MachineOperand(MachineOperand::VREG, SymbolTable::getLabel());
}

MachineOperand* Instruction::genMachineImm(int val)
{
    return new MachineOperand(MachineOperand::IMM, val);
}

MachineOperand* Instruction::genMachineLabel(int block_no)
{
    std::ostringstream buf;
    buf << ".L" << block_no;
    std::string label = buf.str();
    return new MachineOperand(label);
}

void AllocaInstruction::genMachineCode(AsmBuilder* builder)
{
    /* HINT:
     * Allocate stack space for local variabel
     * Store frame offset in symbol entry */
    auto cur_func = builder->getFunction();
    int size = se->getType()->getSize() / 8;
    //fprintf(stdout,"size:%d\n",size);
    if (size < 0)
        size = 4;
    int offset = cur_func->AllocSpace(size);
    dynamic_cast<TemporarySymbolEntry*>(operands[0]->getEntry())->setOffset(-offset);
}

void LoadInstruction::genMachineCode(AsmBuilder* builder)
{
    auto cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;
    // Load global operand
    if (operands[1]->getEntry()->isVariable() && dynamic_cast<IdentifierSymbolEntry*>(operands[1]->getEntry())->isGlobal())
    {
        //fprintf(stdout,"################\n");
        auto dst = genMachineOperand(operands[0]);
        auto internal_reg1 = genMachineVReg();
        auto internal_reg2 = new MachineOperand(*internal_reg1);
        auto src = genMachineOperand(operands[1]);
        // example: load r0, addr_a
        cur_inst = new LoadMInstruction(cur_block, internal_reg1, src);
        cur_block->InsertInst(cur_inst);
        // example: load r1, [r0]
        cur_inst = new LoadMInstruction(cur_block, dst, internal_reg2);
        cur_block->InsertInst(cur_inst);
    }
    // Load local operand
    else if (operands[1]->getEntry()->isTemporary() && operands[1]->getDef() && operands[1]->getDef()->isAlloc())
    {
        // example: load r1, [r0, #4]
        auto dst = genMachineOperand(operands[0]);
        auto src1 = genMachineReg(11);
        int off = dynamic_cast<TemporarySymbolEntry*>(operands[1]->getEntry())->getOffset();
        auto src2 = genMachineImm(off);
        if(!islegel(off))
        {
            auto operand = genMachineVReg();
            cur_block->InsertInst((new LoadMInstruction(cur_block, operand, src2)));
            src2 = operand;
        }
        cur_inst = new LoadMInstruction(cur_block, dst, src1, src2);
        cur_block->InsertInst(cur_inst);
    }
    // Load operand from temporary variable
    else
    {
        // example: load r1, [r0]
        auto dst = genMachineOperand(operands[0]);
        auto src = genMachineOperand(operands[1]);
        cur_inst = new LoadMInstruction(cur_block, dst, src);
        cur_block->InsertInst(cur_inst);
    }
}

void StoreInstruction::genMachineCode(AsmBuilder* builder)
{
    auto cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;
    auto dst = genMachineOperand(operands[0]);
    auto src = genMachineOperand(operands[1]);
    // store immediate
    if (operands[1]->getEntry()->isConstant())
    {
        //fprintf(stdout,"#####################\n");
        auto tmp = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, tmp, src);
        cur_block->InsertInst(cur_inst);
        // src = tmp;
        src = new MachineOperand(*tmp);
    }
    // store to local
    if (operands[0]->getEntry()->isTemporary() && operands[0]->getDef() && operands[0]->getDef()->isAlloc())
    {
        auto src1 = genMachineReg(11);
        int offnum = dynamic_cast<TemporarySymbolEntry*>(operands[0]->getEntry())->getOffset();
        auto src2 = genMachineImm(offnum);
        if(!islegel(offnum))
        {
            auto operand = genMachineVReg();
            cur_block->InsertInst((new LoadMInstruction(cur_block, operand, src2)));
            src2 = operand;
        }
        cur_inst = new StoreMInstruction(cur_block, src, src1, src2);
        cur_block->InsertInst(cur_inst);
    }
    // store to global
    else if (operands[0]->getEntry()->isVariable() && dynamic_cast<IdentifierSymbolEntry*>(operands[0]->getEntry())->isGlobal())
    {
        auto internal_reg1 = genMachineVReg();
        // example: load r0, addr_a
        cur_inst = new LoadMInstruction(cur_block, internal_reg1, dst);
        cur_block->InsertInst(cur_inst);
        // example: store r1, [r0]
        cur_inst = new StoreMInstruction(cur_block, src, internal_reg1);
        cur_block->InsertInst(cur_inst);
    }
    // store to pointer
    else if (operands[0]->getType()->isPtr())
    {
        cur_inst = new StoreMInstruction(cur_block, src, dst);
        cur_block->InsertInst(cur_inst);
    }
}

void BinaryInstruction::genMachineCode(AsmBuilder* builder)
{
    // complete other instructions
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);
    auto src1 = genMachineOperand(operands[1]);
    auto src2 = genMachineOperand(operands[2]);
    /* HINT:
     * The source operands of ADD instruction in ir code both can be immediate
     * num. However, it's not allowed in assembly code. So you need to insert
     * LOAD/MOV instrucrion to load immediate num into register. As to other
     * instructions, such as MUL, CMP, you need to deal with this situation,
     * too.*/
    MachineInstruction* cur_inst = nullptr;
    if (src1->isImm())
    {
        auto internal_reg = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, internal_reg, src1);
        cur_block->InsertInst(cur_inst);
        src1 = new MachineOperand(*internal_reg);
    }
    if (src2->isImm())
    {
        if ((opcode <= BinaryInstruction::OR && ((ConstantSymbolEntry*)(operands[2]->getEntry()))->getValue() > MAXINT) || opcode >= BinaryInstruction::MUL)
        {
            auto internal_reg = genMachineVReg();
            cur_inst = new LoadMInstruction(cur_block, internal_reg, src2);
            cur_block->InsertInst(cur_inst);
            src2 = new MachineOperand(*internal_reg);
        }
    }
    switch (opcode)
    {
        case ADD:
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, dst, src1, src2);
            break;
        case SUB:
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::SUB, dst, src1, src2);
            break;
        case AND:
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::AND, dst, src1, src2);
            break;
        case OR:
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::OR,dst, src1, src2);
            break;
        case MUL:
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::MUL, dst, src1, src2);
            break;
        case DIV:
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::DIV, dst, src1, src2);
            break;
        case MOD:
        {
            // 12/1
            // c = a % b
            // c = a / b
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::DIV, dst, src1, src2);
            MachineOperand* dst1 = new MachineOperand(*dst);
            src1 = new MachineOperand(*src1);
            src2 = new MachineOperand(*src2);
            cur_block->InsertInst(cur_inst);
            // c = c * b
            cur_inst = new BinaryMInstruction(
                cur_block, BinaryMInstruction::MUL, dst1, dst, src2);
            cur_block->InsertInst(cur_inst);
            dst = new MachineOperand(*dst1);
            // c = a - c
            cur_inst = new BinaryMInstruction(
                cur_block, BinaryMInstruction::SUB, dst, src1, dst1);
            break;
        }
        default:
            break;
    }
    cur_block->InsertInst(cur_inst);
}

void CmpInstruction::genMachineCode(AsmBuilder* builder)
{
    auto cur_block = builder->getBlock();
    auto src1 = genMachineOperand(operands[1]);
    auto src2 = genMachineOperand(operands[2]);
    MachineInstruction* cur_inst = nullptr;
    if (src1->isImm())
    {
        auto temp_reg = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, temp_reg, src1);
        cur_block->InsertInst(cur_inst);
        src1 = new MachineOperand(*temp_reg);
    }
    if (src2->isImm() && ((ConstantSymbolEntry*)(operands[2]->getEntry()))->getValue() > MAXINT)//不能两个都是立即数。
    {
        auto temp_reg = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, temp_reg, src2);
        cur_block->InsertInst(cur_inst);
        src2 = new MachineOperand(*temp_reg);
    }
    cur_inst = new CmpMInstruction(cur_block, src1, src2, opcode);
    cur_block->InsertInst(cur_inst);
    if (opcode >= CmpInstruction::L && opcode <= CmpInstruction::GE)//结果存到两个寄存器里比较。
    {
        auto dst = genMachineOperand(operands[0]);
        auto trueOperand = genMachineImm(1);
        auto falseOperand = genMachineImm(0);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, trueOperand, opcode);
        cur_block->InsertInst(cur_inst);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, falseOperand, 7 - opcode);
        cur_block->InsertInst(cur_inst);
    }
}

void UncondBrInstruction::genMachineCode(AsmBuilder* builder)
{
    auto cur_block = builder->getBlock();
    std::stringstream strout;
    strout << ".L" << branch->getNo();
    MachineOperand* dst = new MachineOperand(strout.str());
    auto cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::B, dst);
    cur_block->InsertInst(cur_inst);
}

void CondBrInstruction::genMachineCode(AsmBuilder* builder)
{
    auto cur_block = builder->getBlock();
    std::stringstream s;
    s << ".L" << true_branch->getNo();
    MachineOperand* dst = new MachineOperand(s.str());
    auto cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::B, dst, cur_block->getCmpOp());
    cur_block->InsertInst(cur_inst);
    s.str("");
    s << ".L" << false_branch->getNo();
    dst = new MachineOperand(s.str());
    cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::B, dst);
    cur_block->InsertInst(cur_inst);
}

void RetInstruction::genMachineCode(AsmBuilder* builder)
{
    // TODO
    /* HINT:
     * 1. Generate mov instruction to save return value in r0
     * 2. Restore callee saved registers and sp, fp
     * 3. Generate bx instruction */
    auto cur_block = builder->getBlock();
    if (!operands.empty())
    {
        auto dst = new MachineOperand(MachineOperand::REG, 0);
        auto src = genMachineOperand(operands[0]);
        auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
        cur_block->InsertInst(cur_inst);
    }
    auto cur_func = builder->getFunction();
    auto sp = new MachineOperand(MachineOperand::REG, 13);
    auto size = new MachineOperand(MachineOperand::IMM, cur_func->getAllocSpace());
    auto cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, sp, sp, size);
    cur_block->InsertInst(cur_inst);
    auto lr = new MachineOperand(MachineOperand::REG, 14);
    auto cur_inst2 = new BranchMInstruction(cur_block, BranchMInstruction::BX, lr);
    cur_block->InsertInst(cur_inst2);
}

void CallInstruction::genMachineCode(AsmBuilder* builder) 
{
    auto cur_block = builder->getBlock();
    MachineOperand* operand;
    MachineInstruction* cur_inst;
    int pnum = 0;
    for (auto it = operands.begin(); it != operands.end(); it++, pnum++) 
    {
        if (pnum == 0)
            continue;
        if (pnum == 5)
            break;
        operand = genMachineReg(pnum - 1);
        auto src = genMachineOperand(operands[pnum]);
        if (src->isImm() && src->getVal() > MAXINT)
            cur_inst = new LoadMInstruction(cur_block, operand, src);
        else
            cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, operand, src);
        cur_block->InsertInst(cur_inst);
    }// 12/5 四个以内的参数，用寄存器传参。
    for (int i = operands.size() - 1; i > 4; i--)// 12/1 超过的参数压栈。
    {
        operand = genMachineOperand(operands[i]);
        if (operand->isImm())
        {
            auto tmp = genMachineVReg();
            if (operand->getVal() <= MAXINT)
                cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, tmp, operand);
            else
                cur_inst = new LoadMInstruction(cur_block, tmp, operand);
            cur_block->InsertInst(cur_inst);
            operand = tmp;
        }
        std::vector<MachineOperand*> vec;
        cur_inst = new StackMInstrcuton(cur_block, StackMInstrcuton::PUSH, vec, operand);
        cur_block->InsertInst(cur_inst);
    }
    auto label = new MachineOperand(func->toStr().c_str());
    cur_inst = new BranchMInstruction(cur_block, BranchMInstruction::BL, label);
    cur_block->InsertInst(cur_inst);
    if (operands.size() > 5)
    {
        auto off = genMachineImm((operands.size() - 5) * 4);
        auto sp = new MachineOperand(MachineOperand::REG, 13);// 12/5 栈指针归位。
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, sp, sp, off);
        cur_block->InsertInst(cur_inst);
    }
    if (dst!=nullptr)// 12/5 存储函数返回值。
    {
        operand = genMachineOperand(dst);
        auto r0 = new MachineOperand(MachineOperand::REG, 0);
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, operand, r0);// 12/5 将返回值从r0中mov。
        cur_block->InsertInst(cur_inst);
    }
}


void NotInstruction::genMachineCode(AsmBuilder* builder) 
{
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);
    auto trueOperand = genMachineImm(1);
    auto falseOperand = genMachineImm(0);
    auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, trueOperand, MachineInstruction::EQ);
    cur_block->InsertInst(cur_inst);
    cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, falseOperand, MachineInstruction::NE);
    cur_block->InsertInst(cur_inst);
}

void ZextInstruction::genMachineCode(AsmBuilder* builder) 
{
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);
    auto src = genMachineOperand(operands[1]);
    auto cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
    cur_block->InsertInst(cur_inst);
}

void PtrInstruction::genMachineCode(AsmBuilder* builder)
{
    //fprintf(stdout,"#############\n");
    auto cur_block = builder->getBlock();
    MachineInstruction* cur_inst;
    auto dst = genMachineOperand(operands[0]);
    auto idx = genMachineOperand(operands[2]);

    if(init)
    {
        if(last)
        {
            auto tmpBase = genMachineOperand(init);
            cur_inst = new BinaryMInstruction(
                cur_block, BinaryMInstruction::ADD, dst, tmpBase, genMachineImm(4));
            cur_block->InsertInst(cur_inst);
        }
        return;
    }
    
    MachineOperand* base = nullptr;
    int size;
    auto tmp_index = genMachineVReg();
    if (idx->isImm())
    {
        if (idx->getVal() < MAXINT)
            cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, tmp_index, idx);
        else
            cur_inst = new LoadMInstruction(cur_block, tmp_index, idx);
        idx = new MachineOperand(*tmp_index);
        cur_block->InsertInst(cur_inst);
    }

    if (paramFirst)
        size = ((PointerType*)(operands[1]->getType()))->getType()->getSize() / 8;
    else
    {
        if (firstDim)
        {
            //fprintf(stdout,"##############\n");
            base = genMachineVReg();
            //全局变量使用。
            if (operands[1]->getEntry()->isVariable() && ((IdentifierSymbolEntry*)(operands[1]->getEntry()))->isGlobal())
            {
                //fprintf(stdout,"###############\n");
                auto src = genMachineOperand(operands[1]);
                cur_inst = new LoadMInstruction(cur_block, base, src);
            }
            else
            {
                int offset = ((TemporarySymbolEntry*)(operands[1]->getEntry()))->getOffset();// 12/5 整个数组的偏移量。
                if (islegel(offset))
                    cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, base, genMachineImm(offset));
                else 
                    cur_inst = new LoadMInstruction(cur_block, base, genMachineImm(offset));
            }
            cur_block->InsertInst(cur_inst);
        }
        ArrayType* type = (ArrayType*)(((PointerType*)(operands[1]->getType()))->getType());
        size = type->getElementType()->getSize() / 8;
    }
    //fprintf(stdout,"%d\n",size);
    auto temp_size = genMachineVReg();
    if (islegel(size))
        cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, temp_size, genMachineImm(size));// 12/5 元素大小。
    else
        cur_inst = new LoadMInstruction(cur_block, temp_size, genMachineImm(size));
    cur_block->InsertInst(cur_inst);
    auto temp_off = genMachineVReg();
    cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::MUL, temp_off, idx, temp_size);//定位维度。
    temp_off = new MachineOperand(*temp_off);
    cur_block->InsertInst(cur_inst);
    if (paramFirst || !firstDim)
    {
        /*if(paramFirst)
            fprintf(stdout,"pf\n");
        if(!first)
            fprintf(stdout,"#############\n");*/
        auto arr = genMachineOperand(operands[1]);
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, dst, arr, temp_off);
        //定位具体位置。不用指定基地址。
        cur_block->InsertInst(cur_inst);
    }
    else
    {
        auto addr = genMachineVReg();
        auto temp_base = new MachineOperand(*base);
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, addr, temp_base, temp_off);
        cur_block->InsertInst(cur_inst);
        addr = new MachineOperand(*addr);
        if (operands[1]->getEntry()->isVariable() && ((IdentifierSymbolEntry*)(operands[1]->getEntry()))->isGlobal())
            cur_inst = new MovMInstruction(cur_block, MovMInstruction::MOV, dst, addr);//全局变量，直接mov。
        else
        {
            auto fp = genMachineReg(11);
            cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD, dst, fp, addr);//栈中数组元素地址。
        }
        cur_block->InsertInst(cur_inst);
    }
    //fprintf(stdout,"%d\n",size);
}
