/* ============================================================
   PROYECTO: Demand Forecasting & Demand Planning
   OBJETIVO: Preparar dataset analítico mensual de demanda
   FUENTE: Kaggle - Demand Forecasting with Tabular Data
   AUTOR: Diego Correa
   MOTOR: MySQL
   ============================================================

   Este script realiza:
   1. Validación inicial de datos
   2. Normalización de tipos de datos
   3. Control de calidad
   4. Construcción de tabla limpia
   5. Agregación mensual de la demanda
   ============================================================ */

-- ============================================================
-- 1. VALIDACIÓN INICIAL DE TABLAS
-- ============================================================

-- Validar volumen de registros
SELECT COUNT(*) AS total_sales FROM sales_train;
SELECT COUNT(*) AS total_items FROM items;
SELECT COUNT(*) AS total_restaurants FROM resturants;

-- ============================================================
-- 2. NORMALIZACIÓN DE TIPOS DE DATOS
-- ============================================================

-- Verificar formato original de la columna date
SELECT DISTINCT date
FROM sales_train
LIMIT 5;

-- Conversión de la columna date a tipo DATE
-- Se asume formato YYYY-MM-DD sin valores inválidos
ALTER TABLE sales_train
MODIFY COLUMN date DATE;

-- item_count representa unidades vendidas
-- Se define como entero por semántica de negocio
ALTER TABLE sales_train
MODIFY COLUMN item_count INT;

-- price representa valor monetario.
-- Se utiliza DECIMAL para evitar errores de precisión y se especifica 2 decimales para optimizar memoria
ALTER TABLE sales_train
MODIFY COLUMN price DECIMAL(10,2);

-- ============================================================
-- 3. CONTROL DE CALIDAD DE DATOS
-- ============================================================

-- Validar fechas nulas
SELECT COUNT(*) AS null_dates
FROM sales_train
WHERE date IS NULL;

-- Validar rango temporal esperado
SELECT MIN(date) AS min_date, MAX(date) AS max_date
FROM sales_train;

-- Validar ventas con item_id inexistente
SELECT COUNT(*) AS invalid_item_id
FROM sales_train s
LEFT JOIN items i 
	ON s.item_id = i.id
WHERE i.id IS NULL;

-- Validar cantidades nulas o negativas
-- Se aceptan ceros como ventas válidas
SELECT COUNT(*) AS invalid_item_count
FROM sales_train
WHERE item_count IS NULL OR item_count < 0;

-- ============================================================
-- 4. CONSTRUCCIÓN DE TABLA LIMPIA
-- ============================================================

-- Se crea una tabla limpia excluyendo registros inválidos
-- La tabla original se conserva sin modificaciones lógicas
DROP TABLE IF EXISTS sales_clean;

CREATE TABLE sales_clean AS
SELECT *
FROM sales_train
WHERE
    date IS NOT NULL
    AND item_id IS NOT NULL
    AND item_count IS NOT NULL
    AND item_count >= 0;

-- Validación Post-Limpieza / Se compara los registros originales vs los registros de la tabla nueva
SELECT COUNT(*) AS original_rows FROM sales_train;
SELECT COUNT(*) AS clean_rows FROM sales_clean;

-- ============================================================
-- 5. AGREGACIÓN MENSUAL (DATASET ANALÍTICO)
-- ============================================================

-- Agregación de demanda mensual por producto.
-- Variable objetivo: unidades vendidas (item_count)

DROP TABLE IF EXISTS monthly_demand;

CREATE TABLE monthly_demand AS
SELECT
    YEAR(s.date) AS year,
    MONTH(s.date) AS month,
    s.item_id,
    i.store_id,
    SUM(s.item_count) AS monthly_units_sold
FROM sales_clean s
JOIN items i 
	ON s.item_id = i.id
GROUP BY
    YEAR(s.date),
    MONTH(s.date),
    s.item_id,
    i.store_id
ORDER BY
    s.item_id, year, month;

-- Conversión de la columna monthly_units_sold a tipo INT
ALTER TABLE monthly_demand
MODIFY COLUMN monthly_units_sold INT;

-- ============================================================
-- 6. VALIDACIÓN DE LA SERIE TEMPORAL
-- ============================================================

-- Verificar cantidad de meses observados por producto
-- Útil para detectar series incompletas
SELECT
    item_id,
    COUNT(*) AS months_observed
FROM monthly_demand
GROUP BY item_id
ORDER BY months_observed DESC;