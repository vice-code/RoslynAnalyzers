using System.Collections.Generic;
using System.Linq;

using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ViceCode.Analyzers.Utils
{
    internal static class Helper
    {
        internal static List<PropertyDeclarationSyntax> GetClassUnsetProperties(TypeDeclarationSyntax typeDeclaration, ConstructorDeclarationSyntax constructorDeclaration, bool ignoreGetOnly = false)
        {
            IEnumerable<PropertyDeclarationSyntax> properties = typeDeclaration.Members.OfType<PropertyDeclarationSyntax>();          // Only properties.
            List<PropertyDeclarationSyntax> listProperties = properties.ToList();                                               // Properties that are not set in the constructor.

            if (ignoreGetOnly)
            {
                foreach (PropertyDeclarationSyntax property in properties)
                {
                    if (property.AccessorList.Accessors.Count == 1 && property.AccessorList.Accessors[0].Kind() == SyntaxKind.GetAccessorDeclaration)
                    {
                        listProperties.Remove(property);
                    }
                }
            }

            if (constructorDeclaration.Body is null)
                return listProperties;

            // Search for properties that are not set in the constructor.
            foreach (StatementSyntax statement in constructorDeclaration.Body.Statements)
            {
                if (statement.Kind() != SyntaxKind.ExpressionStatement)
                {
                    continue;
                }

                ExpressionStatementSyntax expression = (ExpressionStatementSyntax)statement;

                if (expression.Expression.Kind() != SyntaxKind.SimpleAssignmentExpression)
                {
                    continue;
                }

                AssignmentExpressionSyntax assigment = (AssignmentExpressionSyntax)expression.Expression;

                if (assigment.Left.Kind() != SyntaxKind.IdentifierName)
                {
                    continue;
                }

                IdentifierNameSyntax assigmentLeftIdentifierName = (IdentifierNameSyntax)assigment.Left;

                foreach (PropertyDeclarationSyntax property in properties)
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
