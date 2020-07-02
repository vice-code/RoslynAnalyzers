using System.Collections.Generic;
using System.Linq;

using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ViceCode.Analyzers
{
    internal static class Helper
    {
        internal static List<PropertyDeclarationSyntax> GetClassUnsetProperties(ClassDeclarationSyntax classDeclaration, ConstructorDeclarationSyntax constructorDeclaration)
        {
            var properties = classDeclaration.Members.OfType<PropertyDeclarationSyntax>();          // Only properties.
            var listProperties = properties.ToList();                                               // Properties that are not set in the constructor.

            if (constructorDeclaration.Body is null)
                return listProperties;

            // Search for properties that are not set in the constructor.
            foreach (var statement in constructorDeclaration.Body.Statements)
            {
                if (statement.Kind() != SyntaxKind.ExpressionStatement)
                {
                    continue;
                }

                var expression = (ExpressionStatementSyntax)statement;

                if (expression.Expression.Kind() != SyntaxKind.SimpleAssignmentExpression)
                {
                    continue;
                }

                var assigment = (AssignmentExpressionSyntax)expression.Expression;

                if(assigment.Left.Kind() != SyntaxKind.IdentifierName)
                {
                    continue;
				}

                var assigmentLeftIdentifierName = (IdentifierNameSyntax)assigment.Left;

                foreach (var property in properties)
                {
                    if (SyntaxFactory.AreEquivalent(assigmentLeftIdentifierName.Identifier, property.Identifier))
                    {
                        // Remove the properties from the list, since it is set in the constructor.
                        listProperties.Remove(property);
                        break;      // Go to next property assigment
                    }
                }
            }

            return listProperties;
        }
    }
}
