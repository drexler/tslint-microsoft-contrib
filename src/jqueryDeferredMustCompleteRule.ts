import * as ts from 'typescript';
import * as Lint from 'tslint';
import * as tsutils from 'tsutils';

import { AstUtils } from './utils/AstUtils';
import { Utils } from './utils/Utils';
import { ExtendedMetadata } from './utils/ExtendedMetadata';

export class Rule extends Lint.Rules.AbstractRule {
    public static metadata: ExtendedMetadata = {
        ruleName: 'jquery-deferred-must-complete',
        type: 'maintainability',
        description:
            'When a JQuery Deferred instance is created, then either reject() or resolve() must be called on it within all code branches in the scope.',
        options: null, // tslint:disable-line:no-null-keyword
        optionsDescription: '',
        typescriptOnly: true,
        issueClass: 'Non-SDL',
        issueType: 'Error',
        severity: 'Critical',
        level: 'Opportunity for Excellence',
        group: 'Correctness'
    };

    public static FAILURE_STRING: string =
        'A JQuery deferred was found that appears to not have resolve or reject invoked on all code paths: ';

    public apply(sourceFile: ts.SourceFile): Lint.RuleFailure[] {
        return this.applyWithFunction(sourceFile, walk);
    }
}

function isPromiseInstantiation(expression: ts.Expression): boolean {
    if (expression !== undefined && expression.kind === ts.SyntaxKind.CallExpression) {
        const functionName = AstUtils.getFunctionName(<ts.CallExpression>expression);
        const functionTarget = AstUtils.getFunctionTarget(<ts.CallExpression>expression);

        if (functionName === 'Deferred' && functionTarget !== undefined && AstUtils.isJQuery(functionTarget)) {
            return true;
        }
    }
    return false;
}

function isCompletionFunction(functionName: string): boolean {
    return /^(resolve|reject)$/.test(functionName);
}

function walk(ctx: Lint.WalkContext<void>) {
    function cb(node: ts.Node): void {
        if (tsutils.isBinaryExpression(node)) {
            if (node.operatorToken.getText() === '=' && isPromiseInstantiation(node.right)) {
                if (node.left.kind === ts.SyntaxKind.Identifier) {
                    if ((<ts.Identifier>node.left).text !== undefined) {
                        const name: ts.Identifier = <ts.Identifier>node.left;
                        validateDeferredUsage(node, name);
                    }
                }
            }
        }

        if (tsutils.isVariableDeclaration(node)) {
            if (node.initializer !== undefined && isPromiseInstantiation(node.initializer)) {
                if ((<ts.Identifier>node.name).text !== undefined) {
                    const name: ts.Identifier = <ts.Identifier>node.name;
                    validateDeferredUsage(node, name);
                }
            }
        }

        return ts.forEachChild(node, cb);
    }

    return ts.forEachChild(ctx.sourceFile, cb);

    function validateDeferredUsage(rootNode: ts.Node, deferredIdentifier: ts.Identifier): void {
        const parent: ts.Node = AstUtils.findParentBlock(rootNode);
        const isBlockComplete = analyzeBlock(ctx, parent, deferredIdentifier);
        if (!isBlockComplete) {
            const failureString = Rule.FAILURE_STRING + "'" + rootNode.getText() + "'";
            ctx.addFailureAt(rootNode.getStart(), rootNode.getWidth(), failureString);
        }
    }
}

function analyzeBlock(ctx: Lint.WalkContext<void>, parent: ts.Node, deferredIdentifier: ts.Identifier): boolean {
    let wasCompleted: boolean = false;
    let allBranchesCompleted: boolean = true; // by default, there are no branches, so this is true
    let hasBranches: boolean = false;

    cb(parent);

    if (wasCompleted) {
        return true; // if the main code path completed then it doesn't matter what the child branches did
    }
    if (!hasBranches) {
        return false; // if there were no branches and it is not complete... then it is in total not complete.
    }

    return allBranchesCompleted;

    function cb(node: ts.Node): void {
        if (tsutils.isIfStatement(node)) {
            hasBranches = true;

            // an if statement is a branch, so we need to see if this branch completes.
            const ifBranchCompletes: boolean = analyzeBlock(ctx, node.thenStatement, deferredIdentifier);
            if (!ifBranchCompletes) {
                allBranchesCompleted = false;
            } else if (node.elseStatement !== undefined) {
                const elseBranchCompletes: boolean = analyzeBlock(ctx, node.elseStatement, deferredIdentifier);

                if (!elseBranchCompletes) {
                    allBranchesCompleted = false;
                }
            }
        }

        if (tsutils.isCallExpression(node)) {
            if (node.expression.kind === ts.SyntaxKind.PropertyAccessExpression) {
                const prop: ts.PropertyAccessExpression = <ts.PropertyAccessExpression>node.expression;

                if (AstUtils.isSameIdentifer(deferredIdentifier, prop.expression)) {
                    const functionName: string = prop.name.getText(); // possibly resolve or reject
                    if (isCompletionFunction(functionName)) {
                        wasCompleted = true;
                        return; // this branch was completed, do not walk any more.
                    }
                }
            }

            const referenceEscaped: boolean = Utils.exists(
                node.arguments,
                (argument: ts.Expression): boolean => {
                    return AstUtils.isSameIdentifer(deferredIdentifier, argument);
                }
            );

            if (referenceEscaped) {
                wasCompleted = true;
                return; // this branch was completed, do not walk any more.
            }

            return ts.forEachChild(node, cb);
        }

        if (tsutils.isArrowFunction(node) || tsutils.isFunctionExpression(node)) {
            const isDeferredShadowed: boolean = Utils.exists(
                node.parameters,
                (param: ts.ParameterDeclaration): boolean => {
                    return AstUtils.isSameIdentifer(deferredIdentifier, param.name);
                }
            );

            if (isDeferredShadowed) {
                hasBranches = true;
                allBranchesCompleted = false;
                return; // this branch was completed, do not walk any more.
            }

            return ts.forEachChild(node, cb);
        }
    }
}
