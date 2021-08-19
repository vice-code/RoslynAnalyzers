using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ViceCode.Analyzers.Utils
{
    internal static class Helper
    {
        internal static List<PropertyDeclarationSyntax> GetTypeDeclarationUnsetProperties(TypeDeclarationSyntax typeDeclaration, ConstructorDeclarationSyntax constructorDeclaration, bool ignoreGetOnly = false)
        {
            IEnumerable<PropertyDeclarationSyntax> properties = typeDeclaration.Members.OfType<PropertyDeclarationSyntax>(); // Only properties.
            List<PropertyDeclarationSyntax> listProperties = properties.ToList(); // Properties that are not set in the constructor.

            if (ignoreGetOnly)
            {
                foreach (PropertyDeclarationSyntax property in properties)
                {
                    if (property.AccessorList is null)
                        continue;

                    if ((property.AccessorList.Accessors.Count == 1 && property.AccessorList.Accessors[0].IsKind(SyntaxKind.GetAccessorDeclaration))
                        || (property.ExpressionBody is not null))
                    {
                        listProperties.Remove(property);
                    }
                }
            }

            if (constructorDeclaration.Body is null)
                return listProperties;

            // Search for properties that are not set in the constructor.
            foreach (ExpressionStatementSyntax expression in constructorDeclaration.Body.DescendantNodes().OfType<ExpressionStatementSyntax>())
            {
                if (!expression.Expression.IsKind(SyntaxKind.SimpleAssignmentExpression))
                    continue;

                AssignmentExpressionSyntax assigment = (AssignmentExpressionSyntax)expression.Expression;

                if (!assigment.Left.IsKind(SyntaxKind.IdentifierName))
                    continue;

                IdentifierNameSyntax assigmentLeftIdentifierName = (IdentifierNameSyntax)assigment.Left;

                foreach (PropertyDeclarationSyntax property in properties)
                {
                    if (SyntaxFactory.AreEquivalent(assigmentLeftIdentifierName.Identifier, property.Identifier))
                    {
                        // Remove the properties from the list, since it is set in the constructor.
                        listProperties.Remove(property);
                        break; // Go to next property assigment
                    }
                }
            }

            return listProperties;
        }
    }
}
