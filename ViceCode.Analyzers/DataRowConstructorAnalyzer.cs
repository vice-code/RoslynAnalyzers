using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

using System;
using System.Collections.Immutable;
using System.Data;
using System.Linq;

namespace ViceCode.Analyzers
{
	[DiagnosticAnalyzer(LanguageNames.CSharp)]
	public class DataRowConstructorAnalyzer : DiagnosticAnalyzer
	{
		public const string CreateDataRowConstructorDiagnosticId = "VC0000";
		public const string UpdateDataRowConstructorDiagnosticId = "VC0001";

		private static readonly LocalizableString TitleCreate = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
		private static readonly LocalizableString MessageFormatCreate = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
		private static readonly LocalizableString DescriptionCreate = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
		private static readonly LocalizableString TitleUpdate = new LocalizableResourceString(nameof(Resources.AnalyzerTitleUpdate), Resources.ResourceManager, typeof(Resources));
		private static readonly LocalizableString MessageFormatUpdate = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormatUpdate), Resources.ResourceManager, typeof(Resources));
		private static readonly LocalizableString DescriptionUpdate = new LocalizableResourceString(nameof(Resources.AnalyzerDescriptionUpdate), Resources.ResourceManager, typeof(Resources));

		private const string Category = "Usage";

		private static DiagnosticDescriptor CreateRule = new DiagnosticDescriptor(CreateDataRowConstructorDiagnosticId, TitleCreate, MessageFormatCreate, Category, DiagnosticSeverity.Info, isEnabledByDefault: true, description: DescriptionCreate);
		private static DiagnosticDescriptor UpdateRule = new DiagnosticDescriptor(UpdateDataRowConstructorDiagnosticId, TitleUpdate, MessageFormatUpdate, Category, DiagnosticSeverity.Info, isEnabledByDefault: true, description: DescriptionUpdate);

		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(CreateRule, UpdateRule); } }

		public override void Initialize(AnalysisContext context)
		{
			// See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
			// <SnippetRegisterNodeAction>
			context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze | GeneratedCodeAnalysisFlags.None);
			context.EnableConcurrentExecution();
			context.RegisterSyntaxNodeAction(AnalyzeNodeForClass, SyntaxKind.ClassDeclaration);
			context.RegisterSyntaxNodeAction(AnalyzeNodeForRecord, SyntaxKind.RecordDeclaration);
			// </SnippetRegisterNodeAction>
		}

		private void AnalyzeNodeForClass(SyntaxNodeAnalysisContext context)
		{
			var classDeclaration = (ClassDeclarationSyntax)context.Node;
			var semanticModel = context.SemanticModel;

			Lazy<INamedTypeSymbol> dataRowNamedTypeSymbol = new Lazy<INamedTypeSymbol>(() => context.Compilation.GetTypeByMetadataName(typeof(DataRow).FullName));

			foreach (var member in classDeclaration.Members)
			{
				if (member.Kind() != SyntaxKind.ConstructorDeclaration)
					continue;

				var constructorDeclaration = (ConstructorDeclarationSyntax)member;
				var parameters = constructorDeclaration.ParameterList.Parameters;
				if (parameters.Count != 1)
				{
					// Нас интересует только один параметр в конструкторе.
					continue;
				}

				var paramSymbol = semanticModel.GetDeclaredSymbol(parameters[0]);
				ITypeSymbol paramTypeSymbol = paramSymbol.Type;

				if (!SymbolEqualityComparer.Default.Equals(dataRowNamedTypeSymbol.Value, paramTypeSymbol))
				{
					// Единственный параметр конструктора не является DataRow, ищем дальше
					continue;
				}

				var listProperties = Helper.GetClassUnsetProperties(classDeclaration, constructorDeclaration);           // Свойства, которые не заданы в конструкторе.

				if (listProperties.Count == 0)
				{
					// Список пуст - нечего добавлять
					return;
				}

				context.ReportDiagnostic(Diagnostic.Create(UpdateRule, constructorDeclaration.Identifier.GetLocation()));
				return;
			}

			var properties = classDeclaration.Members.OfType<PropertyDeclarationSyntax>().ToList();       // Берём только свойства.
			if (properties.Count == 0)
				return;     // нечего устанавливать.

			context.ReportDiagnostic(Diagnostic.Create(CreateRule, classDeclaration.Identifier.GetLocation()));
		}

		private void AnalyzeNodeForRecord(SyntaxNodeAnalysisContext context)
		{
			var recordDeclaration = (RecordDeclarationSyntax)context.Node;
			var semanticModel = context.SemanticModel;

			Lazy<INamedTypeSymbol> dataRowNamedTypeSymbol = new Lazy<INamedTypeSymbol>(() => context.Compilation.GetTypeByMetadataName(typeof(DataRow).FullName));

			foreach (var member in recordDeclaration.Members)
			{
				if (member.Kind() != SyntaxKind.ConstructorDeclaration)
					continue;

				var constructorDeclaration = (ConstructorDeclarationSyntax)member;
				var parameters = constructorDeclaration.ParameterList.Parameters;
				if (parameters.Count != 1)
				{
					// Нас интересует только один параметр в конструкторе.
					continue;
				}

				var paramSymbol = semanticModel.GetDeclaredSymbol(parameters[0]);
				ITypeSymbol paramTypeSymbol = paramSymbol.Type;

				if (!SymbolEqualityComparer.Default.Equals(dataRowNamedTypeSymbol.Value, paramTypeSymbol))
				{
					// Единственный параметр конструктора не является DataRow, ищем дальше
					continue;
				}

				var listProperties = Helper.GetClassUnsetProperties(recordDeclaration, constructorDeclaration);           // Свойства, которые не заданы в конструкторе.

				if (listProperties.Count == 0)
				{
					// Список пуст - нечего добавлять
					return;
				}

				context.ReportDiagnostic(Diagnostic.Create(UpdateRule, constructorDeclaration.Identifier.GetLocation()));
				return;
			}

			var properties = recordDeclaration.Members.OfType<PropertyDeclarationSyntax>().ToList();       // Берём только свойства.
			if (properties.Count == 0)
				return;     // нечего устанавливать.

			context.ReportDiagnostic(Diagnostic.Create(CreateRule, recordDeclaration.Identifier.GetLocation()));
		}
	}
}
