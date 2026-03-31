/**
 * Setup Steps progress widget
 */

import React from 'react';
import * as Types from '../../../shared/types';

interface Props {
  steps: Types.SetupStep[];
}

const SetupStepsWidget: React.FC<Props> = ({ steps }) => {
  const getStepIcon = (status: Types.SetupStepStatus) => {
    switch (status) {
      case Types.SetupStepStatus.COMPLETED:
        return (
          <div className="flex items-center justify-center w-6 h-6 bg-green-500 text-white rounded-full">
            <svg className="w-4 h-4" fill="currentColor" viewBox="0 0 20 20">
              <path fillRule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clipRule="evenodd" />
            </svg>
          </div>
        );
      case Types.SetupStepStatus.IN_PROGRESS:
        return (
          <div className="flex items-center justify-center w-6 h-6 bg-blue-500 text-white rounded-full">
            <div className="w-2 h-2 bg-white rounded-full animate-pulse"></div>
          </div>
        );
      case Types.SetupStepStatus.FAILED:
        return (
          <div className="flex items-center justify-center w-6 h-6 bg-red-500 text-white rounded-full">
            <svg className="w-4 h-4" fill="currentColor" viewBox="0 0 20 20">
              <path fillRule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clipRule="evenodd" />
            </svg>
          </div>
        );
      case Types.SetupStepStatus.NOT_STARTED:
      default:
        return (
          <div className="flex items-center justify-center w-6 h-6 bg-gray-200 text-gray-600 rounded-full">
            {steps.indexOf(steps.find(s => s.step === steps.indexOf(steps) + 1) || steps[0]) + 1}
          </div>
        );
    }
  };

  const sortedSteps = [...steps].sort((a, b) => a.step - b.step);

  return (
    <div className="bg-white rounded-lg shadow p-6">
      <h3 className="text-lg font-semibold text-gray-900 mb-4">Setup Progress</h3>
      <div className="space-y-3">
        {sortedSteps.map((step, index) => (
          <div key={step.step} className="flex items-start space-x-3">
            <div className="flex-shrink-0 pt-1">
              {getStepIcon(step.status)}
            </div>
            <div className="flex-1">
              <p className="text-sm font-medium text-gray-900">{step.title}</p>
              <p className="text-xs text-gray-600 mt-1">{step.description}</p>
              {step.error && (
                <p className="text-xs text-red-600 mt-1">{step.error}</p>
              )}
              {step.completedAt && (
                <p className="text-xs text-gray-500 mt-1">
                  Completed on {new Date(step.completedAt).toLocaleDateString()}
                </p>
              )}
            </div>
            <div className="flex-shrink-0">
              <span className={`inline-flex items-center px-2 py-1 rounded-full text-xs font-medium ${
                step.status === Types.SetupStepStatus.COMPLETED
                  ? 'bg-green-100 text-green-800'
                  : step.status === Types.SetupStepStatus.IN_PROGRESS
                  ? 'bg-blue-100 text-blue-800'
                  : step.status === Types.SetupStepStatus.FAILED
                  ? 'bg-red-100 text-red-800'
                  : 'bg-gray-100 text-gray-800'
              }`}>
                {step.status.replace(/_/g, ' ')}
              </span>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
};

export default SetupStepsWidget;
