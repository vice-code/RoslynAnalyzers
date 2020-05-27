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
            var properties = classDeclaration.Members.OfType<PropertyDeclarationSyntax>();       // Берём только свойства.
            var listProperties = properties.ToList();           // Свойства, которые не заданы в конструкторе.

            // Выполняем поиск свойств, которые не установлены в конструкторе.
            var expressionStatementSyntaxes = constructorDeclaration.Body.Statements.OfType<ExpressionStatementSyntax>();
            foreach (var expression in expressionStatementSyntaxes)
            {
                if (!(expression.Expression is AssignmentExpressionSyntax assigment))
                {
                    // действие не является присваиванием.
                    continue;
                }

                foreach (var property in properties)
                {
                    if (assigment.Left is IdentifierNameSyntax leftNameSyntax && SyntaxFactory.AreEquivalent(leftNameSyntax.Identifier, property.Identifier))
                    {
                        listProperties.Remove(property);
                        // Удаляем свойства из списка, тк оно устанавливается в конструкторе.
                        // Переходим к следующему присваиванию.
                        break;
                    }
                }
            }

            return listProperties;
        }
    }
}
