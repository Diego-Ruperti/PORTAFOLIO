import pandas as pd
import pytest

from src import config
from src.predictor import (
    ValidationError,
    load_cluster_profiles,
    load_pipeline,
    predict_single,
    validate_columns,
)

VALID_WINE = {
    "Alcohol": 14.23,
    "Malic_Acid": 1.71,
    "Ash": 2.43,
    "Ash_Alcanity": 15.6,
    "Magnesium": 127,
    "Total_Phenols": 2.8,
    "Flavanoids": 3.06,
    "Nonflavanoid_Phenols": 0.28,
    "Proanthocyanins": 2.29,
    "Color_Intensity": 5.64,
    "Hue": 1.04,
    "OD280": 3.92,
    "Proline": 1065,
}


@pytest.fixture(scope="module")
def pipeline():
    return load_pipeline(config.MODEL_PATH)


@pytest.fixture(scope="module")
def profiles():
    return load_cluster_profiles(config.CLUSTER_PROFILE_PATH)


def test_validate_columns_accepts_complete_numeric_row():
    df = pd.DataFrame([VALID_WINE])
    assert validate_columns(df) == []


def test_validate_columns_reports_missing_column():
    df = pd.DataFrame([{k: v for k, v in VALID_WINE.items() if k != "Proline"}])
    errors = validate_columns(df)
    assert any("Proline" in e for e in errors)


def test_validate_columns_reports_non_numeric_value():
    df = pd.DataFrame([dict(VALID_WINE, Alcohol="catorce")])
    errors = validate_columns(df)
    assert any("Alcohol" in e and "no numéricos" in e for e in errors)


def test_validate_columns_reports_missing_value():
    df = pd.DataFrame([dict(VALID_WINE, Hue=None)])
    errors = validate_columns(df)
    assert any("Hue" in e and "faltantes" in e for e in errors)


def test_predict_single_returns_known_segment(pipeline, profiles):
    result = predict_single(pipeline, profiles, VALID_WINE)
    assert result["segment"] in {"Premium Reserve", "Classic Balance", "Light & Fresh"}
    assert result["price_range"] in {"$40-60", "$15-25", "$8-15"}


def test_predict_single_raises_on_invalid_input(pipeline, profiles):
    bad_wine = dict(VALID_WINE, Alcohol="catorce")
    with pytest.raises(ValidationError):
        predict_single(pipeline, profiles, bad_wine)


def test_predict_batch_classifies_every_row_and_adds_columns(pipeline, profiles):
    from src.predictor import predict_batch

    df = pd.DataFrame([VALID_WINE, VALID_WINE])
    result = predict_batch(pipeline, profiles, df)
    assert len(result) == 2
    assert {"Cluster", "Segmento", "Precio_Sugerido", "Canal_Sugerido"} <= set(result.columns)


def test_predict_batch_rejects_whole_file_on_any_invalid_row(pipeline, profiles):
    from src.predictor import predict_batch

    df = pd.DataFrame([VALID_WINE, dict(VALID_WINE, Alcohol="catorce")])
    with pytest.raises(ValidationError):
        predict_batch(pipeline, profiles, df)
