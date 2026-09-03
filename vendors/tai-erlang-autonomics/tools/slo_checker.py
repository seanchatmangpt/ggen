#!/usr/bin/env python3
"""
SLO Compliance Checker
Validates SLO thresholds against Google Cloud Monitoring metrics
"""

import argparse
import json
import sys
from datetime import datetime, timedelta

from google.cloud import monitoring_v3, bigquery


def check_uptime_slo(threshold: float, lookback_hours: int = 24) -> dict:
    """Check 99.5% uptime SLO"""
    client = monitoring_v3.MetricServiceClient()
    project_name = client.project_path(get_project_id())

    query = monitoring_v3.ListTimeSeriesRequest(
        name=project_name,
        filter='''
            resource.type="cloud_run_revision"
            AND resource.labels.service_name="tai-autonomics"
            AND metric.type="run.googleapis.com/request_count"
        ''',
        interval=monitoring_v3.TimeInterval(
            start_time=dict(seconds=int(
                (datetime.utcnow() - timedelta(hours=lookback_hours)).timestamp()
            )),
            end_time=dict(seconds=int(datetime.utcnow().timestamp()))
        )
    )

    results = client.list_time_series(request=query)

    total_requests = 0
    failed_requests = 0

    for series in results:
        for point in series.points:
            total_requests += int(point.value.double_value)

    # Query failed requests (5xx)
    failed_query = monitoring_v3.ListTimeSeriesRequest(
        name=project_name,
        filter='''
            resource.type="cloud_run_revision"
            AND resource.labels.service_name="tai-autonomics"
            AND metric.type="run.googleapis.com/request_count"
            AND metric.response_code_class="5xx"
        ''',
        interval=monitoring_v3.TimeInterval(
            start_time=dict(seconds=int(
                (datetime.utcnow() - timedelta(hours=lookback_hours)).timestamp()
            )),
            end_time=dict(seconds=int(datetime.utcnow().timestamp()))
        )
    )

    failed_results = client.list_time_series(request=failed_query)
    for series in failed_results:
        for point in series.points:
            failed_requests += int(point.value.double_value)

    uptime = (total_requests - failed_requests) / total_requests if total_requests > 0 else 1.0

    return {
        'name': 'uptime_slo',
        'threshold': threshold,
        'actual': uptime,
        'passed': uptime >= threshold,
        'total_requests': total_requests,
        'failed_requests': failed_requests,
        'uptime_percent': uptime * 100,
        'lookback_hours': lookback_hours
    }


def check_success_rate_slo(threshold: float, lookback_hours: int = 24) -> dict:
    """Check 99% success rate SLO"""
    client = monitoring_v3.MetricServiceClient()
    project_name = client.project_path(get_project_id())

    # Get total requests
    query = monitoring_v3.ListTimeSeriesRequest(
        name=project_name,
        filter='''
            resource.type="cloud_run_revision"
            AND resource.labels.service_name="tai-autonomics"
            AND metric.type="run.googleapis.com/request_count"
        ''',
        interval=monitoring_v3.TimeInterval(
            start_time=dict(seconds=int(
                (datetime.utcnow() - timedelta(hours=lookback_hours)).timestamp()
            )),
            end_time=dict(seconds=int(datetime.utcnow().timestamp()))
        )
    )

    results = client.list_time_series(request=query)
    total_requests = 0

    for series in results:
        for point in series.points:
            total_requests += int(point.value.double_value)

    # Get 2xx responses
    success_query = monitoring_v3.ListTimeSeriesRequest(
        name=project_name,
        filter='''
            resource.type="cloud_run_revision"
            AND resource.labels.service_name="tai-autonomics"
            AND metric.type="run.googleapis.com/request_count"
            AND metric.response_code_class="2xx"
        ''',
        interval=monitoring_v3.TimeInterval(
            start_time=dict(seconds=int(
                (datetime.utcnow() - timedelta(hours=lookback_hours)).timestamp()
            )),
            end_time=dict(seconds=int(datetime.utcnow().timestamp()))
        )
    )

    success_results = client.list_time_series(request=success_query)
    successful_requests = 0

    for series in success_results:
        for point in series.points:
            successful_requests += int(point.value.double_value)

    success_rate = successful_requests / total_requests if total_requests > 0 else 1.0

    return {
        'name': 'success_rate_slo',
        'threshold': threshold,
        'actual': success_rate,
        'passed': success_rate >= threshold,
        'total_requests': total_requests,
        'successful_requests': successful_requests,
        'success_rate_percent': success_rate * 100,
        'lookback_hours': lookback_hours
    }


def check_latency_slo(threshold: float, lookback_hours: int = 24) -> dict:
    """Check P99 latency < 500ms SLO"""
    bq_client = bigquery.Client()
    project_id = get_project_id()

    query = f"""
    SELECT
      APPROX_QUANTILES(duration_ms, 100)[OFFSET(99)] as p99_latency,
      COUNT(*) as request_count,
      COUNTIF(duration_ms < {threshold}) as fast_requests
    FROM `{project_id}.tai_autonomics_traces.spans`
    WHERE operation_name='http_request'
      AND start_time > TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL {lookback_hours} HOUR)
    """

    results = bq_client.query(query).result()

    for row in results:
        p99_latency = row.p99_latency or 0
        request_count = row.request_count or 0
        fast_requests = row.fast_requests or 0

        pass_rate = fast_requests / request_count if request_count > 0 else 1.0

        return {
            'name': 'latency_slo',
            'threshold': threshold,
            'actual': p99_latency,
            'passed': p99_latency < threshold,
            'p99_latency_ms': p99_latency,
            'request_count': request_count,
            'fast_requests': fast_requests,
            'pass_rate_percent': pass_rate * 100,
            'lookback_hours': lookback_hours
        }

    return {
        'name': 'latency_slo',
        'threshold': threshold,
        'actual': None,
        'passed': False,
        'error': 'No trace data available'
    }


def get_project_id() -> str:
    """Get GCP project ID from environment"""
    import os
    return os.getenv('GOOGLE_CLOUD_PROJECT', '')


def main():
    parser = argparse.ArgumentParser(
        description='Check SLO compliance against thresholds'
    )
    parser.add_argument(
        '--slo-name',
        required=True,
        choices=['uptime', 'success_rate', 'latency'],
        help='SLO to check'
    )
    parser.add_argument(
        '--threshold',
        type=float,
        required=True,
        help='SLO threshold (e.g., 0.995 for 99.5%)'
    )
    parser.add_argument(
        '--lookback-hours',
        type=int,
        default=24,
        help='Hours to look back for metrics'
    )

    args = parser.parse_args()

    try:
        if args.slo_name == 'uptime':
            result = check_uptime_slo(args.threshold, args.lookback_hours)
        elif args.slo_name == 'success_rate':
            result = check_success_rate_slo(args.threshold, args.lookback_hours)
        elif args.slo_name == 'latency':
            result = check_latency_slo(args.threshold, args.lookback_hours)

        print(json.dumps(result, indent=2))

        if result.get('passed'):
            print(f"\n✅ SLO PASSED: {result['name']}")
            sys.exit(0)
        else:
            print(f"\n❌ SLO FAILED: {result['name']}")
            sys.exit(1)

    except Exception as e:
        print(f"Error checking SLO: {e}", file=sys.stderr)
        sys.exit(2)


if __name__ == '__main__':
    main()
