from src import config


def test_has_thirteen_chemical_columns():
    assert len(config.COLUMN_NAMES) == 13


def test_cluster_rules_cover_three_segments():
    assert set(config.CLUSTER_RULES.keys()) == {
        "premium_reserve",
        "classic_balance",
        "light_fresh",
    }


def test_premium_reserve_price_range():
    assert config.CLUSTER_RULES["premium_reserve"]["price_range"] == "$40-60"


def test_classic_balance_price_range():
    assert config.CLUSTER_RULES["classic_balance"]["price_range"] == "$15-25"


def test_light_fresh_price_range():
    assert config.CLUSTER_RULES["light_fresh"]["price_range"] == "$8-15"


def test_kmeans_hyperparameters_match_spec():
    assert config.N_CLUSTERS == 3
    assert config.N_INIT == 25
    assert config.RANDOM_STATE == 123
