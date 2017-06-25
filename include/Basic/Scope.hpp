#pragma once

#include <memory>
#include <vector>

#include "AST/ASTNode.hpp"

using namespace ast;

using namespace ast::type;

class Scope
{
public:
    Scope(Scope *Outer)
        : mOuterScope(Outer)
        , mCompoundStmt(this) {}

    CompoundStmtNode *getCompoundStmt() {
        return &mCompoundStmt;
    }
/*
    bool operator==(Scope Rhs) const {
        return mCSN == Rhs.mCSN;
    }

    bool operator!=(Scope Rhs) const {
        return !(*this == Rhs);
    }

    bool isNestedOf(Scope S) const {

    }
*/
    Scope *openScope() {
        return mInnerScopes.emplace_back(std::make_unique<Scope>(this)).get();
    }

    Scope *closeScope() {
        return mOuterScope;
    }
/* TODO, leave them for now
    type::Compound *makeCompoundType(std::string_view Name, std::vector<Type> Types) {
        return mCompoundTypes.emplace_back(std::make_unique<type::Compound>(Name, std::move(Types)))
            .get();
    }
*/

    VarDeclNode *buildVarDecl(std::string_view Name, Type Type) {
        auto Node = std::make_unique<VarDeclNode>(this, Name, Type);


        auto *PNode = mVarDecls.emplace_back(Node.get());

        mCompoundStmt.addStmt(std::move(Node));
        
        return PNode;
    }

    ptr<VarDeclRefNode> makeRef(VarDeclNode *Var) {
        return std::make_unique<VarDeclRefNode>(this, Var);
    }

    template <class TConstant>
    ptr<ConstantNode> makeConstant(TConstant Constant) {
        return std::make_unique<ConstantNode>(this, Constant);
    }
/*
    std::pair<CompoundStmtNode *, Scope> makeCompoundStmt(Scope *S) {
        auto *Node = mCompoundStmts.emplace_back(std::make_unique<CompoundStmtNode>(S))
            .get();
        S->addStmt(Node);
        return {Node, {Node}};
    }
    */

    FnNode *buildFunctionDecl(std::string_view Name, Type RetType, std::vector<Type> ParmTypes, ptr<CompoundStmtNode> FnBody) {
        type::Function FnType {RetType, std::move(ParmTypes)};
        auto Node = std::make_unique<FnNode>(this, Name, FnType, std::move(FnBody));
        auto *PNode = Node.get();

        mFunctions.push_back(PNode);

        mCompoundStmt.addStmt(std::move(Node));
        return PNode;
    }

    FnNode *getFunction(std::string_view Name) {
        for (auto *Fn : mFunctions) {
            if (Name == Fn->getName()) {
                return Fn;
            }
        }
        return nullptr;
    }

    CallNode *buildFunctionCall(std::string_view FnName, std::vector<VarOrConstant> CallArgs) {
        auto *Fn = getFunction(FnName);
        if (!Fn) {
            return nullptr;
        }
        auto Node = std::make_unique<CallNode>(this, Fn, std::move(CallArgs));
        
        auto *PNode = Node.get();
        mCompoundStmt.addStmt(std::move(Node));
        return PNode;
    }

    AssignNode *buildAssign(ptr<VarDeclRefNode> Lhs, AssignNode::AssignExpr Rhs) {
        auto Node = std::make_unique<AssignNode>(this, std::move(Lhs), std::move(Rhs));
        auto *PNode = Node.get();
        mCompoundStmt.addStmt(std::move(Node));
        return PNode;
    }

    ptr<RelOpNode> makeCompare(RelOpId Id, RelOpNode::Op1ExprType Lhs, RelOpNode::Op2ExprType Rhs) {
        return std::make_unique<RelOpNode>(this, Id, std::move(Lhs), std::move(Rhs));
    }

    Scope *getOuterScope() {
        return mOuterScope;
    }

    //void ssaReplace(ast::CompoundStmtNode NewCSN, std::vector<std::unique_ptr<VarDeclNode)

private:
    Scope *mOuterScope;
    ast::CompoundStmtNode mCompoundStmt;

    std::vector<std::unique_ptr<Scope>> mInnerScopes;


//    std::vector<ptr<type::Compound>>> mCompoundTypes;
//    std::vector<ptr<type::Function>>> mFunctionTypes;

    std::vector<VarDeclNode *> mVarDecls;
    std::vector<ConstantNode *> mConstants;
    std::vector<VarDeclRefNode *> mVarRefs;
    std::vector<FnNode *> mFunctions;
    std::vector<CallNode *> mFnCalls;
    std::vector<AssignNode *> mAssignments;
    std::vector<RelOpNode *> mComparisons;
    std::vector<CompoundStmtNode *> mCompoundStmts;


    std::vector<OPNode> mNodes;


};
