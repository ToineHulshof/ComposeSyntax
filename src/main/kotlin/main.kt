import kastree.ast.Node
import kastree.ast.Visitor
import kastree.ast.psi.Converter
import kastree.ast.psi.Parser
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.utils.fileUtils.withReplacedExtensionOrNull
import java.io.File

val ignoredParameters = listOf("ScaffoldState", "Modifier") // "MaterialTheme"
val ignoredArguments = ignoredParameters.map { it.decapitalize() } + listOf("checked", "overflow", "items", "value", "onCloseRequest", "confirmButton")
val ignoredViews = listOf("MaterialTheme", "Surface", "ProvideEmphasis", "ThemedPreview")
val ignoredParametersForViews = listOf("Scaffold")
val ignoredModifiers = listOf("ripple", "gravity")
val modifierArguments = listOf("style", "color", "scaleFit", "shape", "maxLines", "onValueChange", "backgroundColor")
val viewsWithNormalChildren = listOf("MaterialTheme", "Crossfade", "Surface", "VerticalScroller", "HorizontalScroller", "Column", "Row", "Stack", "ProvideEmphasis", "Clickable", "forEach", "ThemedPreview", "IconToggleButton", "Card", "Box", "TabRow", "Toggleable", "IconButton")

// No support for inline variables, because SwiftUI doesn't support them (yet)
// When there is a inline variable (for complicated modifier for example), the user should replace the variable.

// Cannot implement alerts unfortunately

fun main(args: Array<String>) {
    // Optional flag for including @EnvironmentObject in every view. Standard not.
    // Optional flag for setting default values instead of variables for constants for example. Standard on.
    // Check if there is an argument. Otherwise display help
    if (args.isEmpty()) {
        println("Please enter a file or directory path")
        return
    }
    val file = File(args[0])

    file.walk().forEach {
        if (it.extension == "kt") {
            val translation = translateFile(it)
            val outputFile = File(it.path.replace("jetnews", "swiftui")).withReplacedExtensionOrNull("kt", "swift") ?: return@forEach
            outputFile.parentFile.mkdirs()
            if (outputFile.createNewFile()) {
                outputFile.writeText(translation)
                println("Translated ${it.path}")
                return@forEach
            }
            println("Failed to translate ${it.path}")
        }
    }
}

fun translateFile(file: File): String {
    val text = file.readText()
    val code = parseCodeWithText(text)
    val translations = mutableListOf<String>()
    Visitor.visit(code) { v, _ ->
        when (v) {
            is Node.Decl.Func -> {
                val annotations = v.mods.mapNotNull { (it as? Node.Modifier.AnnotationSet)?.anns?.flatMap { it.names } }.flatten()
                if (annotations.contains("Composable")) {
                    translations.add(translateComposable(v, annotations.contains("Preview"), 1))
                } else {
                    val tag = v.tag as? String ?: return@visit
                    translations.add(tag)
                }
            }
            is Node.Decl.Structured -> {
                val annotations = v.mods.mapNotNull { (it as? Node.Modifier.AnnotationSet)?.anns?.flatMap { it.names } }.flatten()
                if (annotations.contains("Model")) {
                    translations.add(translateModel(v))
                }
            }
        }
    }
//    print(translations.joinToString("\n\n"))
    return translations.joinToString("\n\n")
}

fun parseCodeWithText(code: String) = Parser(object : Converter() {
    override fun onNode(node: Node, elem: PsiElement) {
        node.tag = elem.text.replace(" =", ":").replace("?:", "??")//.replace("it", "$0")
    }
}).parseFile(code)

fun translateModel(model: Node.Decl.Structured): String {
    var s = "class ${model.name}: ObservableObject {\n"
    s += model.members.filterIsInstance<Node.Decl.Property>().filter { it.readOnly }.joinToString("\n") {
        var property = "\t"
        property += "@Published var " + it.vars.firstOrNull()?.name + " = "
        when (val expr = it.expr) {
            is Node.Expr.Call -> {
                (expr.expr as? Node.Expr.Name)?.let {
                    if (it.name == "ModelList") property += "[${expr.typeArgs?.firstOrNull()?.ref?.tag}]()"
                }
            }
        }
        return@joinToString property
    }
    s += "\n}"
    return s
}

fun translateComposable(func: Node.Decl.Func, isPreview: Boolean, depth: Int): String {
    var s = ""
    val parameters = func.params.filter { !ignoredParameters.contains((it.type?.ref as? Node.TypeRef.Simple)?.pieces?.map { it.name }?.firstOrNull()) }
    val statements = (func.body as? Node.Decl.Func.Body.Block)?.block?.stmts ?: return s
    val declarations = statements.filterIsInstance<Node.Stmt.Decl>()
    val expressions = statements.filterIsInstance<Node.Stmt.Expr>()

    s += "struct ${func.name}: " + (if (isPreview) "PreviewProvider" else "View") + " {\n"
    s += if (parameters.isNotEmpty()) parameters.joinToString("\n", postfix = "\n\n") { tab(depth) + translateProperty(it).replace("Boolean", "Bool").replace("Unit", "Void") } else ""
    s += if (declarations.isNotEmpty()) declarations.joinToString("\n", postfix = "\n\n") { tab(depth) + translateDeclaration(it) } else ""
    s += tab(depth) + "${if (isPreview) "static " else ""}var ${if (isPreview) "previews" else "body"}: some View {\n"
    s += if (expressions.size > 1) tab(depth + 1) + "VStack(alignment: .leading) {\n" else ""
    s += if (expressions.isNotEmpty()) expressions.joinToString("\n") { translateBody(it.expr, null, expressions.size, depth + if (expressions.size > 1) 2 else 1) } else ""
    s += if (expressions.size > 1) "\n" + tab(depth + 1) + "}" else ""
    s += "\n" + tab(depth) + "}" + if (parameters.size + declarations.size != 0) "\n" else ""
    s += "\n" + tab(depth - 1) + "}"
//    if (expressions.size > 1) s = wrapInVStack(s, true, depth)
    return s
}

// Could add support for something like this: post.image?.let { image ->
fun translateBody(expr: Node.Expr, parent: String?, childrenCount: Int, depth: Int): String {
    var isForEach = false
    when (expr) {
        is Node.Expr.When -> {
            if (parent == "Surface") { return expr.entries.mapNotNull { it.body as? Node.Expr.Call }.joinToString("\n") { translateTabItem(it, depth + 1) } }
            return expr.entries.joinToString("\n" + tab(depth), tab(depth)) { "if ${expr.expr?.tag} == ${it.conds.firstOrNull()?.tag} { ${translateBody(it.body, parent, 1, 0)} }" }
        }
        is Node.Expr.If -> {
            var s = ""
            val stmts = (expr.body as? Node.Expr.Brace)?.block?.stmts ?: return s
            s += tab(depth) + "if ${expr.expr.tag} {\n" + stmts.mapNotNull { it as? Node.Stmt.Expr }.joinToString("\n") { translateBody(it.expr, parent, stmts.size, depth + 1) } + "\n" + tab(depth) + "}"
            expr.elseBody?.let {
                val elseStmts = (it as? Node.Expr.Brace)?.block?.stmts ?: return s
                s += " else {\n" + elseStmts.mapNotNull { it as? Node.Stmt.Expr }.joinToString("\n") { translateBody(it.expr, parent, elseStmts.size, depth + 1) } + "\n" + tab(depth) + "}"
            }
            return if (childrenCount == 1) wrapInVStack(s, false, depth) else s
        }
        is Node.Expr.BinaryOp -> {
            if (((expr.rhs as? Node.Expr.Call)?.expr as? Node.Expr.Name)?.name == "forEach") {
                isForEach = true
            } else {
                // if let unfortunately not supported in SwiftUI (yet) (it really should)
                var s = ""
                val lambda = (expr.rhs as? Node.Expr.Call)?.lambda ?: return s
                val variable = lambda.func.params.firstOrNull()?.vars?.firstOrNull()?.name ?: return s
                s += tab(depth) + "if let $variable = ${expr.lhs.tag} {\n"
                val stmts = lambda.func.block?.stmts ?: return s
                s += stmts.mapNotNull { it as? Node.Stmt.Expr }.joinToString("\n") { translateBody(it.expr, parent, stmts.size, depth + 1) }
                s += "\n" + tab(depth) + "}"
                return s
            }
        }
    }
    var s = ""
    val call = (if (isForEach) (expr as? Node.Expr.BinaryOp)?.rhs else expr) as? Node.Expr.Call ?: return s
    val name = (call.expr as? Node.Expr.Name)?.name ?: return s
    val translatedName = translateName(name)
    val arguments = getArguments(call, name).toMutableList()
    val modifiers = getModifiers(call).toMutableList()
    val children = getChildren(call, name)
    val shouldIgnore = ignoredViews.contains(name)

    arguments.reversed().forEach {
        if (modifierArguments.contains(it.name)) {
            modifiers.add(0, Pair(it.name ?: "", it.expr))
            arguments.remove(it)
        }
    }

    if (name == "Tab") { return tab(depth) + (call.args.firstOrNull()?.expr as? Node.Expr.Brace)?.block?.stmts?.firstOrNull()?.tag as? String ?: "" }

    if (!shouldIgnore) {
        s += tab(depth) + translatedName
        if (isForEach) {
            val argument = call.lambda?.func?.params?.firstOrNull()?.vars?.firstOrNull()?.name ?: "it"
            val list = ((expr as? Node.Expr.BinaryOp)?.lhs as? Node.Expr.Name)?.name ?: return s
            s += "($list, id: \\.self) { $argument in\n"
        }
        s += if (children.isEmpty() || arguments.isNotEmpty()) arguments.joinToString(", ","(", ")") { translateArgument(name, it) } else ""
    }
    if (children.isNotEmpty()) {
        var childrenS = ""
        s += if (!shouldIgnore && !isForEach) " {\n" else ""
        if (name == "TabRow") {
            val items = call.args.firstOrNull { it.name == "items" } ?: return s
            childrenS += tab(depth + 1) + "ForEach(${items.expr.tag}, id: \\.self) { ${call.lambda?.func?.params?.getOrNull(1)?.vars?.firstOrNull()?.name} in\n"
        }
        childrenS += children.joinToString("\n") { translateBody(it.expr, name, children.size, depth + if (name == "TabRow") 2 else if (!shouldIgnore) 1 else 0) }
        childrenS += if (name == "Scaffold") "\n" + tab(depth + 2) + ".navigationBarTitle(Text(${getTitle(call)}))" else ""
        if (name == "TabRow") childrenS += "\n" + tab(depth + 1) + "}"
        childrenS += if (!shouldIgnore) "\n" + tab(depth) + "}" else ""
        if (isForEach && children.size > 1) childrenS = wrapInVStack(childrenS, false, depth)
        s += childrenS
    }
    val shouldInsertSpacer = modifiers.removeAll { it.first == "weight" }
    s += if (name == "Clickable") "\n" + tab(depth + 1) + ".buttonStyle(PlainButtonStyle())" else ""
    s += if (name == "IconToggleButton") "\n" + tab(depth + 1) + ".padding()" else ""
    s += if (name == "Icon") "\n" + tab(depth + 1) + ".imageScale(.large)" else ""
    s += if (name == "Image") "\n" + tab(depth + 1) + ".resizable()" else ""
    s += if (name == "Card") "\n" + tab(depth + 1) + ".overlay(RoundedRectangle(cornerRadius: 4).stroke(Color.init(white: 0.25), lineWidth: 1))" else ""
    s += if (name == "TabRow") "\n" + tab(depth + 1) + ".pickerStyle(SegmentedPickerStyle())\n" + tab(depth + 1) + ".padding()" else ""
    s += if (modifiers.isNotEmpty()) modifiers.mapNotNull { translateModifier(it, depth + 1) }.joinToString("") { "\n" + tab(depth + 1) + "." + it } else ""
    if (shouldInsertSpacer) s += "\n" + tab(depth) + "Spacer()"
    return s
}

fun translateArgument(name: String, argument: Node.ValueArg): String {
    // Eigenlijk moeten de whens omgekeerd. Dus eerst name checken en daarna pas type.
    return when (val expr = argument.expr) {
        is Node.Expr.Name -> {
            when (name) {
                "IconToggleButton" -> if (argument.name == "onBookmark") "action: ${expr.name}" else ""
                "TabRow" -> ""
                "IconButton" -> if (argument.name == "onClick") "action: ${expr.name}" else ""
                else -> {
                    argument.name?.let { return it + ": " + expr.name }
                    expr.name
                }
            }
        }
        is Node.Expr.Brace -> {
            when (name) {
                "Clickable" -> "destination: " + expr.tag as? String //expr.block?.stmts?.joinToString { ((it as? Node.Stmt.Expr)?.expr as? Node.Expr.Call)?.args?.joinToString { "destination: " + ((((it.expr as? Node.Expr.BinaryOp)?.rhs as? Node.Expr.Call)?.expr as? Node.Expr.Name)?.name ?: "") } ?: "" } ?: ""
                "AlertDialog" -> "title: " + expr.block?.stmts?.filterIsInstance<Node.Stmt.Expr>()?.joinToString { translateBody(it.expr, name, expr.block?.stmts?.size ?: 0, 0) }
                else -> argument.tag as? String ?: ""
            }
        }
        is Node.Expr.StringTmpl -> translateString(expr)
        is Node.Expr.Call -> {
            when (name) {
                "Image", "Icon", "IconToggleButton" -> {
                    val type = (expr.expr as? Node.Expr.Name)?.name ?: name
                    when (type) {
                        "vectorResource" -> "systemName: \"" + (((expr.args.firstOrNull()?.expr as? Node.Expr.BinaryOp)?.rhs as? Node.Expr.Name)?.name ?: expr.args.joinToString { "${it.tag}" }) + "\"" // + (if (name == "Icon") "\\(isBookmarked ? \".fill\" : \"\")" else "")
                        "imageResource" -> "\"" + ((expr.args.firstOrNull()?.expr as? Node.Expr.BinaryOp)?.rhs as? Node.Expr.Name)?.name + "\""
                        else -> type
                    }
                }
                else -> argument.tag as? String ?: ""
            }
        }
        is Node.Expr.BinaryOp -> {
            when (name) {
                "TabRow" -> "selection: \$${expr.lhs.tag}, label: Text(\"\")"
                else -> expr.tag as? String ?: ""
            }
        }
        else -> expr.tag as? String ?: ""
    }
}

fun translateModifier(modifier: Pair<String, Node.Expr>, depth: Int): String? {
    val (name, expr) = modifier
    if (ignoredModifiers.contains(name)) return null
    // Could add support for non Const values
    return when (name) {
        "padding" -> {
            val call = expr as? Node.Expr.Call ?: return null
            val sides = call.args.filter { it.name != null }.map { "." + translateSide(it.name ?: "") }
            name + "(" + (if (sides.isNotEmpty()) (if (sides.size == 1) sides.joinToString { it } else "$sides") + ", " else "") + (((call.args.firstOrNull()?.expr as? Node.Expr.BinaryOp)?.lhs as? Node.Expr.Const)?.value ?: "10") + ")"
        }
        "style" -> {
            val category = ((expr as? Node.Expr.BinaryOp)?.rhs as? Node.Expr.Name)?.name ?: return expr.tag as? String ?: ""
            translateTextModifier(category, depth)
        }
        "color" -> "foregroundColor(.secondary)"
        "preferredSize" -> {
            val sizes = (expr as? Node.Expr.Call)?.args?.mapNotNull { ((it.expr as? Node.Expr.BinaryOp)?.lhs as? Node.Expr.Const)?.value } ?: return expr.tag as? String
            "frame(width: ${sizes.getOrElse(0) { "0" }}, height: ${sizes.getOrElse(1) { "0" }})"
        }
        "preferredHeight" -> "frame(height: ${(((expr as? Node.Expr.Call)?.args?.firstOrNull()?.expr as? Node.Expr.BinaryOp)?.lhs as? Node.Expr.Const)?.value ?: "100"})"
        "preferredWidth" -> "frame(width: ${(((expr as? Node.Expr.Call)?.args?.firstOrNull()?.expr as? Node.Expr.BinaryOp)?.lhs as? Node.Expr.Const)?.value ?: "100"})"
        "preferredHeightIn" -> "frame(minHeight: ${(((expr as? Node.Expr.Call)?.args?.firstOrNull()?.expr as? Node.Expr.BinaryOp)?.lhs as? Node.Expr.Const)?.value ?: "100"})"
        "preferredWidthIn" -> "frame(minWidth: ${(((expr as? Node.Expr.Call)?.args?.firstOrNull()?.expr as? Node.Expr.BinaryOp)?.lhs as? Node.Expr.Const)?.value ?: "100"})"
        "fillMaxSize" -> "frame(minWidth: 0, maxWidth: .infinity, minHeight: 0, maxHeight: .infinity)"
        "fillMaxWidth" -> "frame(minWidth: 0, maxWidth: .infinity)"
        "fillMaxHeight" -> "frame(minHeight: 0, maxHeight: .infinity)"
        "scaleFit" -> "scaledToFit()" // "aspectRatio(contentMode: .fill)"
        "clip" -> "clipped()\n" + tab(depth) + ".cornerRadius(${((((expr as? Node.Expr.Call)?.args?.firstOrNull()?.expr as? Node.Expr.Call)?.args?.firstOrNull()?.expr as? Node.Expr.BinaryOp)?.lhs as? Node.Expr.Const)?.value ?: 10})"
        "maxLines" -> "lineLimit(${(expr as? Node.Expr.Const)?.value ?: "nil"})"
        "shape" -> "clipShape(${(expr as? Node.Expr.Name)?.name ?: expr.tag}())"//"cornerRadius(${(((expr as? Node.Expr.Call)?.args?.firstOrNull()?.expr as? Node.Expr.BinaryOp)?.lhs as? Node.Expr.Const)?.value ?: "10"})"
        "onValueChange" -> "onTapGesture(perform: ${(expr as? Node.Expr.Name)?.name ?: expr.tag})\n"
        "backgroundColor" -> "background(Color.${(expr as? Node.Expr.Name)?.name ?: expr.tag})"
        else -> name
    }
}

fun translateTextModifier(category: String, depth: Int): String {
    return when (category) {
        "subtitle1" -> "bold()\n" + tab(depth) + ".font(.headline)"
        "overline" -> "font(.caption)\n" + tab(depth) + ".fontWeight(.light)"
        "body2" -> "font(.subheadline)\n" + tab(depth) + ".fontWeight(.light)"
        "h6" -> "bold()\n" + tab(depth) + ".font(.system(size: 20))"
        "h4" -> "bold()\n" + tab(depth) + ".font(.title)"
        "caption" -> "font(.caption)\n" + tab(depth) + ".fontWeight(.light)"
        else -> category
    }
}

fun translateSide(dir: String): String {
    return when (dir) {
        "start" -> "leading"
        "end" -> "trailing"
        else -> dir
    }
}

// Maybe include @State translations
fun translateDeclaration(declaration: Node.Stmt.Decl): String {
    var s = ""
    val property = declaration.decl as? Node.Decl.Property ?: return s
    ((property.expr as? Node.Expr.Call)?.expr as? Node.Expr.Name)?.name?.let {
        when (it) {
            "state" -> s += "@State private "
        }
    }
    s += (if (property.readOnly) "let" else "var") + " " + property.vars.filterIsInstance<Node.Decl.Property.Var>().joinToString { it.name } + " = "
    val expression = property.expr ?: return s
    s += when (expression) {
        is Node.Expr.Const -> expression.value
        else -> {
            (property.expr as? Node.Expr.Call)?.lambda?.let {
                it.func.block?.tag
            } ?: run {
                property.expr?.tag
            }
        } // Could check if it is MaterialTheme or Modifier.
    }
    return s
}

fun translateProperty(property: Node.Decl.Func.Param): String {
    var s = "let "
    val piece = (property.type?.ref as? Node.TypeRef.Simple)?.pieces?.firstOrNull() ?: return s + property.tag
    s += property.name + ": " + if (piece.name == "List") "[${(piece.typeParams.firstOrNull()?.ref as? Node.TypeRef.Simple)?.pieces?.firstOrNull()?.name ?: ""}]" else piece.name
    return s
}

fun translateName(name: String): String {
    return when (name) {
        "Scaffold" -> "NavigationView"
        "Crossfade" -> "TabView"
        "VerticalScroller" -> "ScrollView"
        "HorizontalScroller" -> "ScrollView(.horizontal, showsIndicators: false)"
        "Column" -> "VStack(alignment: .leading)"
        "Row" -> "HStack"
        "Stack" -> "ZStack"
        "Clickable" -> "NavigationLink"
        "forEach" -> "ForEach"
        "IconToggleButton" -> "Button"
        "Icon" -> "Image"
        "Card" -> "Group"
        "TabRow" -> "Picker"
        "Box" -> "Group"
        "Tab" -> "Text"
        "Toggleable" -> "Group"
        "IconButton" -> "Button"
        "AlertDialog" -> "Alert"
        else -> name
    }
}

fun translateTabItem(tabItem: Node.Expr.Call, depth: Int): String {
    var s = ""
    s += translateBody(tabItem, null, 1, depth - 1) + "\n"
    s += tab(depth) + ".tabItem {\n"
    s += tab(depth + 1) + "Image(systemName: \"house\")\n"
    s += tab(depth + 1) + "Text(\"${(tabItem.expr as? Node.Expr.Name)?.name}\")\n"
    s += tab(depth) + "}"
    return s
}

// Could add support from string concatenation with +
fun translateString(string: Node.Expr.StringTmpl): String {
    var s = ""
    s += string.elems.joinToString("") {
        when (it) {
            is Node.Expr.StringTmpl.Elem.Regular -> it.str
            is Node.Expr.StringTmpl.Elem.LongTmpl -> "\\(" + it.expr.tag + ")"
            else -> ""
        }
    }
    return "\"" + s + "\""
}

// Na de vertaling moet de gebruiker even control+option+command+F gebruiken zodat er namen aan argumenten worden gegeven.
// Dit zou te omzeilen zijn met overal een init toevoegen, maar dat is meer moeite en lelijk
fun getArguments(call: Node.Expr.Call, name: String): List<Node.ValueArg> {
    if (ignoredParametersForViews.contains(name)) return emptyList()
    return call.args.filter { !((it.tag as? String)?.startsWith("modifier", true) ?: false) && !ignoredArguments.contains(it.name) && !ignoredArguments.contains((it.expr as? Node.Expr.Name)?.name) && !ignoredParameters.contains(((it.expr as? Node.Expr.BinaryOp)?.lhs as? Node.Expr.Name)?.name)}
}

fun getChildren(call: Node.Expr.Call, name: String): List<Node.Stmt.Expr> {
    when (name) {
        "Scaffold" -> {
            val bodyContent = ((call.args.firstOrNull { it.name == "bodyContent" })?.expr as? Node.Expr.Brace)?.block?.stmts?.filterIsInstance<Node.Stmt.Expr>() ?: emptyList()
            val bottomAppBar = ((call.args.firstOrNull { it.name == "bottomAppBar" })?.expr as? Node.Expr.Brace)?.block?.stmts?.filterIsInstance<Node.Stmt.Expr>() ?: emptyList()
            return bodyContent + bottomAppBar
        }
//        "AlertDialog" -> return ((call.args.firstOrNull { it.name == "text" })?.expr as? Node.Expr.Brace)?.block?.stmts?.filterIsInstance<Node.Stmt.Expr>() ?: emptyList()
        else -> {
            if (viewsWithNormalChildren.contains(name)) {
                return call.lambda?.func?.block?.stmts?.mapNotNull { it as? Node.Stmt.Expr } ?: emptyList()
            }
            return emptyList()
        }
    }
}

// Should add support for modifiers concatenated with . instead of +
fun getModifiers(call: Node.Expr.Call): List<Pair<String, Node.Expr>> {

    fun getModifierFromBinaryOp(op: Node.Expr?): List<Node.Expr> {
        (op as? Node.Expr.Call)?.let { return listOf(it) }
        val binaryOp = op as? Node.Expr.BinaryOp ?: return emptyList()
        val token = (binaryOp.oper as? Node.Expr.BinaryOp.Oper.Token)?.token?.name ?: return emptyList()
        return when (token) {
            "ADD" -> listOf(getModifierFromBinaryOp(binaryOp.lhs), getModifierFromBinaryOp(binaryOp.rhs)).flatten()
            else -> {
                (binaryOp.lhs as? Node.Expr.BinaryOp)?.let {
                    return listOf(getModifierFromBinaryOp(binaryOp.lhs), getModifierFromBinaryOp(binaryOp.rhs)).flatten()
                }
                listOf(binaryOp.rhs)
            }
        }
    }

    // De startsWithCheck is niet compleet veilig, maar anders moet er een complexe recursieve functie worden gemaakt.
    return call.args.filter { (it.tag as? String)?.startsWith("modifier", true) ?: false }.map { it.expr }.filterIsInstance<Node.Expr.BinaryOp>().flatMap { getModifierFromBinaryOp(it).map { Pair(((it as? Node.Expr.Call)?.expr as? Node.Expr.Name)?.name ?: "", it) } }
}

fun getTitle(call: Node.Expr.Call): String {
    val topAppBar = (call.args.firstOrNull { it.name == "topAppBar" }?.expr as? Node.Expr.Brace)?.block?.stmts?.firstOrNull() ?: return ""
    val title = (((topAppBar as? Node.Stmt.Expr)?.expr as? Node.Expr.Call)?.args?.firstOrNull { it.name == "title" }?.expr as? Node.Expr.Brace)?.block?.stmts?.firstOrNull() ?: return ""
    val text = (((title as? Node.Stmt.Expr)?.expr as? Node.Expr.Call)?.args?.firstOrNull()?.expr as? Node.Expr.StringTmpl) ?: return ""
    return translateString(text)
}

fun wrapInVStack(s: String, align: Boolean, depth: Int):String {
    return tab(depth + 1) + "VStack" + (if (align) "(alignment: .leading)" else "") + " {\n" + s.lines().joinToString("\n" + tab(), tab()) { it } + "\n" + tab(depth) + "}"
}

fun tab(depth: Int = 1): String {
    return (1..depth).joinToString("") { "\t" }
}