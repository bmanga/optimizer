/*
 * Define the possible nodes that we can see

  $hello : int32;
  $hello = 44;
 *
 */
#pragma once

#include <variant>
#include <optional>
#include <vector>
#include <string>
#include <cstdint>
#include <memory>

template <class T>
using ptr = std::unique_ptr<T>;



template <class T, template<class...> class Targ, template<class> class Transformer>
struct type_transform;

template <template <class...> class T,
          template<class...> class Targ,
          template<class> class Transformer,
          class... Us>
struct type_transform<T<Us...>, Targ, Transformer>
{
    using type = Targ<typename Transformer<Us>::type...>;
};

template <class T> struct to_raw_transformer
{
    using type = T*;
};

template <class T> struct to_ptr_transformer
{
    using type = ptr<T>;
};

template <class...>
struct ast_types_t{};



class Scope;

namespace ast {
class Context;

namespace type {

class UInt32 {};
class Int32 {};
class Float32 {};
class Function;
class Compound;
} // namespace type
using Type = std::variant<type::UInt32, type::Int32, type::Float32, type::Compound *>;

class type::Compound {
public:
    Compound(std::string_view Name, std::vector<Type> Types)
        : mName(Name)
        , mTypes(std::move(Types)) {}
private:
    std::string mName;
    std::vector<Type> mTypes;
};

class type::Function {
public:
    Function(Type RetType, std::vector<Type> ParmTypes)
        : mRetType(RetType)
        , mParmTypes(std::move(ParmTypes)) {}
private:
    Type mRetType;
    std::vector<Type> mParmTypes;
};



using ast_types = ast_types_t<
    class VarDeclNode,
    class VarDeclRefNode,
    class ConstantNode,
    class FnNode,
    class CallNode,
    class AssignNode,
    class RelOpNode,
    class UnaryLogicOpNode,
    class BinaryLogicOpNode,
    class CondNode,
    class BinaryArithOpNode,
    class UnaryArithOpNode,
    class LoopNode,
    class CompoundStmtNode
    >;

using PNode = typename type_transform<ast_types, std::variant, to_raw_transformer>::type;
using OPNode = typename type_transform<ast_types, std::variant, to_ptr_transformer>::type;




namespace impl {
template <class Arith>
using ConstTargetType =
    std::conditional_t<std::is_floating_point_v<Arith>,
        long double,
        std::conditional_t<std::is_signed_v<Arith>,
            int64_t,
            uint64_t
        >
    >;
} // namespace impl


class NodeBase
{
public:
    NodeBase(Scope * S) : mScope(S) {}

    Scope *getScope() const {
        return mScope;
    }

    void setPrevStmt(PNode Prev) {
        mPrev = Prev;
    }

    void setNextStmt(PNode Next) {
        mNext = Next;
    }
private:
    std::optional<PNode> mPrev;
    std::optional<PNode> mNext;
    Scope *mScope;
};

class ConstantNode : public NodeBase
{
public:
    using ConstantValueType = std::variant<int64_t, uint64_t, long double>;

    template <class Arith>
    ConstantNode(Scope * S, Arith Val)
        : NodeBase(S)
        , mValue{static_cast<impl::ConstTargetType<Arith>>(Val)}
            {}
private:
    ConstantValueType mValue;
};
// $varname : type 



class VarDeclNode : public NodeBase
{
public:
    VarDeclNode(Scope * S, std::string_view Name, Type Type) 
        : NodeBase(S)
        , mName(Name)
        , mType(std::move(Type)) {}

    std::string_view getName() const {
        return mName;
    }

    Type getType() const {
        return mType;
    }
private:
    std::string mName;
    Type mType;
};

class VarDeclRefNode : public NodeBase
{
public:
    VarDeclRefNode(Scope * S, VarDeclNode *VarDecl)
        : NodeBase(S)
        , mVarDecl(VarDecl) {}

    VarDeclNode *getDeclaration() {
        return mVarDecl;
    }
private:
    VarDeclNode *mVarDecl;
};

class FnNode : public NodeBase
{
public:
    FnNode(Scope * S, std::string_view Name, type::Function Type, ptr<CompoundStmtNode> Body)
        : NodeBase(S)
        , mName(Name)
        , mType(Type)
        , mBody(std::move(Body)) {}
    const std::string &getName() const {
        return mName;
    }
private:
    std::string mName;
    type::Function mType;
    ptr<CompoundStmtNode> mBody;
};

using VarOrConstant = std::variant<ptr<VarDeclRefNode>, ptr<ConstantNode>>;

class CallNode : public NodeBase
{
public:
    CallNode(Scope * S, FnNode *Target, std::vector<VarOrConstant> CallArgs)
        : NodeBase(S)
        , mTarget(Target)
        , mCallArgs(std::move(CallArgs)) {}
private:
    FnNode *mTarget;
    std::vector<VarOrConstant> mCallArgs;
};


template <class TLhs, class TRhs>
class BinaryOpBase
{
public:
    using LeftOpType = TLhs;
    using RightOpType = TRhs;
    BinaryOpBase(LeftOpType Lhs, RightOpType Rhs)
        : mLhs(std::move(Lhs))
        , mRhs(std::move(Rhs)) {}

    std::pair<TLhs, TRhs> getDecomposedOperands() {
        return {mLhs, mRhs};
    }

    void replaceLhs(TLhs New) {
        mLhs = std::move(New);
    }

    void replaceRhs(TRhs New) {
        mRhs = std::move(New);
    }

protected:
    LeftOpType mLhs;
    RightOpType mRhs;
};

template <class T>
class UnaryOpBase
{
public:
    using OpType = T;
    UnaryOpBase(OpType Op)
        :mOp(std::move(Op)) {}

    OpType getOperand() {
        return mOp;
    }

    void replaceOperand(OpType Op) {
        mOp = std::move(Op);
    }
private:
protected:
    OpType mOp;
};

template <class T, template<class...> class Base>
struct derives_from_template {
    using Yes = struct {};
    using No  = struct {};

    static No test(...);
    template <class ...Ts>
    static Yes test(Base<Ts...>&);

    constexpr static bool value = std::is_same_v<Yes, decltype(test(std::declval<T>()))>;
};

template <class T>
static constexpr bool is_unary_op_v = derives_from_template<T, UnaryOpBase>::value;

template <class T>
static constexpr bool is_binary_op_v = derives_from_template<T, BinaryOpBase>::value;



class AssignNode
    : public NodeBase
    , public BinaryOpBase<ptr<VarDeclRefNode>,
        std::variant<ptr<VarDeclRefNode>,
                     ptr<ConstantNode>,
                     ptr<RelOpNode>,
                     ptr<BinaryLogicOpNode>,
                     ptr<UnaryLogicOpNode>,
                     ptr<BinaryArithOpNode>,
                     ptr<UnaryArithOpNode>>>
{
public:
    using AssignExpr = BinaryOpBase::RightOpType;
    AssignNode(Scope * S, ptr<VarDeclRefNode> Lhs, AssignExpr Rhs)
        : NodeBase(S)
        , BinaryOpBase(std::move(Lhs), std::move(Rhs)) {}
};

enum class RelOpId
{
    NOT_EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL
};

class RelOpNode
    : public NodeBase
    , public BinaryOpBase<VarOrConstant, VarOrConstant>
{
public:
    using Op1ExprType = VarOrConstant;
    using Op2ExprType = VarOrConstant;

    RelOpNode(Scope * S, RelOpId Id, Op1ExprType Op1, Op2ExprType Op2)
        : NodeBase(S)
        , BinaryOpBase(std::move(Op1), std::move(Op2))
        , mOpId(Id) {}
private:
    RelOpId mOpId;
};

namespace LogicOpId
{
enum Unary : uint8_t{
    NEGATE = 0
};
enum Binary : uint8_t{
    AND_AND = 1,
    OR_OR = 2
};
}

class BinaryLogicOpNode
    : public NodeBase
    , public BinaryOpBase<VarOrConstant, VarOrConstant>
{
public:
    using Op1ExprType = VarOrConstant;
    using Op2ExprType = VarOrConstant;

    BinaryLogicOpNode(Scope * S, LogicOpId::Binary Id, Op1ExprType Op1, Op2ExprType Op2)
        : NodeBase(S)
        , BinaryOpBase(std::move(Op1), std::move(Op2))
        , mOpId(Id) {}
private:
    uint8_t mOpId;
};

class UnaryLogicOpNode
    : public NodeBase
    , public UnaryOpBase<VarOrConstant>
{
public:
    using OpExprType = UnaryOpBase::OpType;

    UnaryLogicOpNode(Scope * S, LogicOpId::Unary Id, VarOrConstant Op)
        : NodeBase(S)
        , UnaryOpBase(std::move(Op)) {}

private:
    uint8_t mOpId;
};

class CondNode : public NodeBase
{
public:
    using CondExprType = std::variant<ptr<VarDeclRefNode>, ptr<RelOpNode>, ptr<BinaryLogicOpNode>, ptr<UnaryLogicOpNode>>;
    CondNode(Scope * S, CondExprType CondExpr, ptr<CompoundStmtNode> TrueBranch, ptr<CompoundStmtNode> FalseBranch)
        : NodeBase(S)
        , mCondition(std::move(CondExpr))
        , mTrueBranch(std::move(TrueBranch))
        , mFalseBranch(std::move(FalseBranch)) {}
private:
    CondExprType mCondition;
    ptr<CompoundStmtNode> mTrueBranch;
    ptr<CompoundStmtNode> mFalseBranch;
};

namespace ArithOpId {
enum Unary : uint8_t
{
    PRE_PLUS_PLUS = 0,
    PRE_MINUS_MINUS = 1,
    POST_PLUS_PLUS = 2,
    POST_MINUS_MINUS = 4
};
enum Binary
{
    PLUS = 8,
    MINUS = 16,
    TIMES = 32,
    DIVIDED = 64,
    MODULUS = 128
};
} // namespace ArithOpId
class BinaryArithOpNode 
    : public NodeBase
    , public BinaryOpBase<VarOrConstant, VarOrConstant>
{
public:
    using Op1ExprType = VarOrConstant;
    using Op2ExprType = VarOrConstant;

    BinaryArithOpNode(Scope * S, ArithOpId::Binary Id, Op1ExprType Op1, Op2ExprType Op2)
        : NodeBase(S)
        , BinaryOpBase(std::move(Op1), std::move(Op2)) {}

private:
    uint8_t mOpId;
};


class UnaryArithOpNode 
    : public NodeBase
    , public UnaryOpBase<VarOrConstant>
{
public:
    using Op1ExprType = VarOrConstant;
    using Op2ExprType = VarOrConstant;
    UnaryArithOpNode(Scope * S, ArithOpId::Unary Id, Op2ExprType Op)
        : NodeBase(S)
        , UnaryOpBase(std::move(Op)) {}

private:
    uint8_t mOpId;
};

class LoopNode : public NodeBase
{
public:
private:
    ptr<CondNode> mLoopCondition;
    ptr<CompoundStmtNode> mLoopBody;
};

class CompoundStmtNode : public NodeBase
{
public:
    CompoundStmtNode(Scope * S)
        : NodeBase(S) {}

    void addStmt(OPNode NewNode) {
        if (!mStatements.empty()) {
            auto &LastNode = mStatements.back();

            std::visit(
                [&LastNode](auto &UPNew, auto &UPLast) {
                    UPNew->setPrevStmt(UPLast.get());
                    UPLast->setNextStmt(UPNew.get());
                }, NewNode, LastNode);
        }
        mStatements.emplace_back(std::move(NewNode));
    }

    /* FIXME make appropriate iterator iterator
    std::vector<PNode> &statements() {
        return mStatements;
    }
    */  
private:
    std::vector<OPNode> mStatements;
};


} // namespace ast


