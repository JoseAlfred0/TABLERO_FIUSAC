# Diccionario de datos (versión inicial)

## Llave de unión seleccionada
- `identificador + carrera` (llave explícita definida para evitar uniones ambiguas).
- En la tabla de notas, si existe `estcarr`, se renombra a `carrera` antes de la unión.
- La unión debe reflejar la relación entre estudiante y carrera, no solo estudiante.

## Mapeo de columnas detectadas
- identificador: `identificador`
- carrera: `carrera`
- id_estudiante: `identificador`
- carnet: `NA`
- curso: `curso`
- nota_final: `nota`
- anio_academico: `ano`
- ciclo: `periodo`
- cohorte: `cohorte`
- estado: `NA`
- area: `area`
- modalidad: `modalidad`

## Nuevas columnas derivadas
- `nota_final`: nota normalizada numérica.
- `anio_academico`: año académico inferido y convertido a entero.
- `ciclo`: periodo académico estandarizado como texto.
- `cohorte`: cohorte inferida cuando existe.
- `estado_academico`: Aprobado/Reprobado/Retirado según regla de negocio.

## Reglas de transformación
- Aprobado si `nota_final >= 61`.
- Reprobado si `nota_final < 61`.
- Retirado si el campo de estado original contiene patrones de retiro/abandono/deserción.

## Validaciones de calidad
- Llaves vacías: 0
- Duplicados por llave: 55522
- Notas fuera de rango [0,100]: 1
- Años fuera de rango [2001,2024]: 0
- Ciclos inconsistentes: 0
- Valores de ciclo no estándar: ninguno

## Advertencias
- Sin advertencias críticas para ETL.
