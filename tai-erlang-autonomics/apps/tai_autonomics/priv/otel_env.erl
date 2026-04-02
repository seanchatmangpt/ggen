%%%-------------------------------------------------------------------
%% @doc OpenTelemetry Environment Configuration
%%      Loads environment-specific OTEL settings for TAIEA
%% @end
%%%-------------------------------------------------------------------

{
  % OTEL SDK settings
  opentelemetry,
  [
    {resource, [
      {service, {name, <<"tai-autonomics">>}},
      {service, {version, <<"1.0.0">>}},
      {deployment, {environment, get_env(<<"ENVIRONMENT">>, <<"production">>)}},
      {cloud, {provider, <<"gcp">>}},
      {cloud, {platform, <<"gcp_cloud_run">>}},
      {service, {instance, {id, get_env(<<"HOSTNAME">>, <<"unknown">>)}}}
    ]},

    % Sampler configuration
    {sampler, {
      opentelemetry_jaeger,
      [
        {sampler, {
          jaeger_remotely_sampled,
          [
            {endpoint, get_env(<<"OTEL_EXPORTER_JAEGER_ENDPOINT">>, <<"http://localhost:14250">>)},
            {sampling_rate, get_env_float(<<"OTEL_SAMPLER_ARG">>, 0.1)}
          ]
        }}
      ]
    }},

    % Span processors
    {processors, [
      {
        opentelemetry_batch_processor,
        [
          {exporter, {
            opentelemetry_exporter,
            [
              {endpoint, get_env(<<"OTEL_EXPORTER_OTLP_ENDPOINT">>, <<"http://localhost:4317">>)},
              {protocol, grpc}
            ]
          }},
          {scheduled_delay_ms, 5000},
          {max_queue_size, 2048},
          {max_export_batch_size, 512}
        ]
      }
    ]},

    % Metric exporter
    {metric_exporters, [
      {
        opentelemetry_metrics_prometheus,
        [
          {port, 8888},
          {path, <<"/metrics">>}
        ]
      }
    ]},

    % Logger integration
    {sdk_logger_config, [
      {handlers, [
        {
          opentelemetry_logger_handler,
          #{
            config => #{
              level => info
            }
          }
        }
      ]}
    ]}
  ]
}
