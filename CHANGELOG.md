# Changelog — Fork local TypeCobol (SQL DML)

Tous les changements notables de ce fork local sont documentés ici.
Ce fichier couvre les commits locaux (travail SQL DML) ainsi que les correctifs upstream fusionnés.

Issue de référence : [TypeCobolTeam/TypeCobol#2837](https://github.com/TypeCobolTeam/TypeCobol/issues/2837)

---

## [En cours] Parsing SQL DML pour COBOL embarqué

### Ajouté — Grammaire et infrastructure SQL DML

- **`b0c7385`** — feat: ajout des règles de grammaire ANTLR et des builders pour `INSERT`, `UPDATE`, `DELETE`, `DECLARE CURSOR`, `SELECT INTO`
- **`d2d38d8`** — feat: câblage des terminaux CUP, builder, dispatcher et listener pour les instructions DML
- **`6ad60b5`** — Fondation SQL DML : nouveaux CodeElements, nœuds AST et infrastructure de visiteur pour toutes les instructions DML
- **`13f2d5b`** — Ajout du plan d'implémentation détaillé pour le parsing SQL DML
- **`af210de`** — Mise à jour de la spécification de design après revue (nommage, grammaire, points d'intégration)
- **`c7969ce`** — Ajout de la spécification de design pour le parsing SQL DML (INSERT/UPDATE/DELETE/DECLARE CURSOR)

### Ajouté — UnsupportedSqlStatement

- **`7275641`** — Ajout de tests pour les instructions SQL non supportées et mise à jour des résultats attendus
- **`c236121`** — Ajout de `UnsupportedSqlStatement` à l'interface `IASTVisitor` (correction d'erreur de build)
- **`e34eaee`** — Ajout de la règle de grammaire `unsupportedSqlStatement` et des productions CUP
- **`475ef48`** — Ajout de `UnsupportedSqlStatement` à l'infrastructure visiteur, builder, dispatcher et listener
- **`b285c1d`** — Ajout du CodeElement et du nœud AST `UnsupportedSqlStatement`

### Ajouté — Récupération d'erreur et configuration

- **`f56af76`** — Ajout des types de tokens DML aux mots-clés de démarrage SQL pour la récupération d'erreur
- **`5103c88`** — Configuration Claude, MCP et documentation du projet

---

## Correctifs upstream fusionnés

Les commits suivants proviennent du dépôt upstream [TypeCobolTeam/TypeCobol](https://github.com/TypeCobolTeam/TypeCobol) et ont été intégrés dans ce fork.

- **`6cd7e75`** — WI #2814 : vérification que REDEFINES ne peut pas être spécifié pour une donnée ayant une clause OCCURS (#2833)
- **`9cce04a`** — WI #2818 : vérification du niveau sur les données ayant une clause OCCURS (#2831)
- **`fde6971`** — WI #2826 : amélioration de la gestion de FILLER dans `InsertVariableDisplay` (#2828)
- **`820d9dd`** — WI #2829 : correction de la définition de type mois/jour pour la date builtin TC (#2830)
- **`83476665`** — WI #2520 : vérification des variables utilisées dans les expressions conditionnelles pour IF / PERFORM / SEARCH (#2827)
- **`a6db112`** — WI #2720 : correction d'une exception NullReference dans `DataLayoutNodeBuilder.From` (#2824)
- **`9d12ca2`** — WI #2696 : correction d'une NullRef dans `AbstractSingleDocumentRefactoring.Run` (#2825)
- **`0e563e2`** — WI #2552 : correction d'une `IndexOutOfRangeException` dans le Scanner lors du parsing de littéraux NationalHexa ou UTF8Hexa (#2822)
- **`e963739`** — WI #2817 : prévention d'une `NullReferenceException` dans la complétion après IN/OF (#2820)
- **`d81e26d`** — WI #2232 : évitement d'une exception d'intervalle de sélection invalide (#2821)
