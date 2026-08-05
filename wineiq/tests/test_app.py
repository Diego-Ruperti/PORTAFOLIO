from streamlit.testing.v1 import AppTest

# AppTest.from_file resolves relative paths against the file that calls it
# (this test module, in tests/), not against the working directory — so the
# path must point back up to the real app.py at the project root.
APP_PATH = "../app.py"

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

VALID_WINE_CSV = (
    "Alcohol,Malic_Acid,Ash,Ash_Alcanity,Magnesium,Total_Phenols,Flavanoids,"
    "Nonflavanoid_Phenols,Proanthocyanins,Color_Intensity,Hue,OD280,Proline\n"
    "14.23,1.71,2.43,15.6,127,2.8,3.06,0.28,2.29,5.64,1.04,3.92,1065\n"
    "13.2,1.78,2.14,11.2,100,2.65,2.76,0.26,1.28,4.38,1.05,3.4,1050\n"
)


def test_app_renders_title_and_three_metrics_without_error():
    at = AppTest.from_file(APP_PATH).run(timeout=30)
    assert not at.exception
    assert at.title[0].value == "🍷 WineIQ"
    # AppTest executes every st.tabs() body in one run, so at.metric also
    # picks up the "Sobre el modelo" tab's 2 metrics — check the header's 3
    # are present by label instead of asserting an exact total count.
    metric_labels = [m.label for m in at.metric]
    assert {"Total de vinos", "Segmentos", "Segmento más común"} <= set(metric_labels)


def test_single_form_classifies_a_valid_wine():
    at = AppTest.from_file(APP_PATH).run(timeout=30)
    for column, value in VALID_WINE.items():
        at.number_input(key=f"input_{column}").set_value(value)
    at.button[0].click().run(timeout=30)

    assert not at.exception
    known_segments = {"Premium Reserve", "Classic Balance", "Light & Fresh"}
    assert any(any(seg in s.value for seg in known_segments) for s in at.success)


def test_process_uploaded_csv_classifies_valid_rows():
    from app import process_uploaded_csv

    classified, errors = process_uploaded_csv(VALID_WINE_CSV.encode("utf-8"))
    assert errors == []
    assert classified is not None
    assert len(classified) == 2
    assert "Segmento" in classified.columns


def test_process_uploaded_csv_rejects_file_with_any_invalid_row():
    from app import process_uploaded_csv

    bad_csv = VALID_WINE_CSV + "catorce,1.71,2.43,15.6,127,2.8,3.06,0.28,2.29,5.64,1.04,3.92,1065\n"
    classified, errors = process_uploaded_csv(bad_csv.encode("utf-8"))
    assert classified is None
    assert len(errors) > 0


def test_model_quality_tab_shows_silhouette_metric():
    at = AppTest.from_file(APP_PATH).run(timeout=60)
    assert not at.exception
    metric_labels = [m.label for m in at.metric]
    assert "Silhouette Score" in metric_labels
