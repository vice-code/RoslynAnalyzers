using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

using System.Collections.Immutable;
using System.Data;
using System.Linq;

namespace ViceCode.Analyzers
{
	[DiagnosticAnalyzer(LanguageNames.CSharp)]
	public class DataRowConstructorAnalyzer : DiagnosticAnalyzer
	{
		public const string CreateDataRowConstructorDiagnosticId = "CreateDataRowconstructor";
		public const string UpdateDataRowConstructorDiagnosticId = "UpdateDataRowconstructor";

		// You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
		// See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
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
			context.RegisterSyntaxNodeAction(AnalyzeNode, SyntaxKind.ClassDeclaration);
			// </SnippetRegisterNodeAction>
		}

		private void AnalyzeNode(SyntaxNodeAnalysisContext context)
		{
			var classDeclaration = (ClassDeclarationSyntax)context.Node;
			var semanticModel = context.SemanticModel;

			var constructors = classDeclaration.Members.OfType<ConstructorDeclarationSyntax>();      // Все определения в классе, являющиеся конструкторами.
																									 // Смотрим все конструкторы, чтобы найти Constructor(DataRow row)
			foreach (ConstructorDeclarationSyntax constructor in constructors)
			{
				var parameters = constructor.ParameterList.Parameters;
				if (parameters.Count != 1)
				{
					// Нас интересует только один параметр в конструкторе.
					continue;
				}

				var dataRowNamedTypeSymbol = context.Compilation.GetTypeByMetadataName(typeof(DataRow).FullName);

				if (dataRowNamedTypeSymbol is null)
				{
					// Не удалось получить информацию о типе DataRow (возможно в зависимостях нет System.Data)
					return;
				}

				var paramSymbol = semanticModel.GetDeclaredSymbol(parameters[0]);
				ITypeSymbol paramTypeSymbol = paramSymbol.Type;

				if (!dataRowNamedTypeSymbol.Equals(paramTypeSymbol))
				{
					// Единственный параметр конструктора не является DataRow, ищем дальше
					continue;
				}

				var listProperties = Helper.GetClassUnsetProperties(classDeclaration, constructor);           // Свойства, которые не заданы в конструкторе.

				if (listProperties.Count == 0)
				{
					// Список пуст - нечего добавлять
					return;
				}

				context.ReportDiagnostic(Diagnostic.Create(UpdateRule, constructor.GetLocation()));
				return;
			}

			var properties = classDeclaration.Members.OfType<PropertyDeclarationSyntax>().ToList();       // Берём только свойства.
			if (properties.Count == 0)
				return;     // нечего устанавливать.

			context.ReportDiagnostic(Diagnostic.Create(CreateRule, context.Node.GetLocation()));
		}
	}
}
