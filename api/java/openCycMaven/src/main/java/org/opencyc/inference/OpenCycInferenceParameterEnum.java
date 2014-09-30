/* $Id: OpenCycInferenceParameterEnum.java 133862 2011-03-16 00:00:42Z daves $
 *
 * Copyright (c) 2004 - 2010 Cycorp, Inc.  All rights reserved.
 * This software is the proprietary information of Cycorp, Inc.
 * Use is subject to license terms.
 */
package org.opencyc.inference;

import java.util.Arrays;
import java.util.List;
import org.opencyc.api.CycObjectFactory;

/**
 * <P>OpenCycInferenceParameterDescription is designed to hard-code a basic set
 * of inference parameters for use with OpenCyc images.
 *
 * <P>Copyright (c) 2004 - 2010 Cycorp, Inc.  All rights reserved.
 * <BR>This software is the proprietary information of Cycorp, Inc.
 * <P>Use is subject to license terms.
 *
 * @author baxter
 * @date August 9, 2005, 8:49 PM
 * @version $Id: OpenCycInferenceParameterEnum.java 133862 2011-03-16 00:00:42Z daves $
 */
public enum OpenCycInferenceParameterEnum {

  INFERENCE_MODE {

    @Override
    InferenceParameter getInferenceParameter() {
      return INFERERNCE_MODE_PARAM;
    }
  },
  MAX_NUMBER {

    @Override
    InferenceParameter getInferenceParameter() {
      return MAX_NUMBER_PARAM;
    }
  },
  MAX_TIME {

    @Override
    InferenceParameter getInferenceParameter() {
      return MAX_TIME_PARAM;
    }
  },
  MAX_TRANSFORMATION_DEPTH {

    @Override
    InferenceParameter getInferenceParameter() {
      return MAX_TRANSFORMATION_DEPTH_PARAM;
    }
  },
  CONTINUABLE {

    @Override
    InferenceParameter getInferenceParameter() {
      return CONTINUABLE_PARAM;
    }
  },
  ALLOW_INDETERMINATE_RESULTS {

    @Override
    InferenceParameter getInferenceParameter() {
      return ALLOW_INDETERMINATE_RESULTS_PARAM;
    }
  };

  abstract InferenceParameter getInferenceParameter();
  private static final InferenceParameter CONTINUABLE_PARAM =
          new DefaultBooleanInferenceParameter(true, InferenceParameters.CONTINUABLE,
          null, "Continuable", "This controls whether the inference is continuable or not.",
          CycObjectFactory.nil, CycObjectFactory.nil, null);
  private static final InferenceParameter ALLOW_INDETERMINATE_RESULTS_PARAM =
          new DefaultBooleanInferenceParameter(false, InferenceParameters.ALLOW_INDETERMINATE_RESULTS,
          null, "Allow result bindings to indeterminates",
          "Whether it is permissible to use instances of #$IndeterminateTerm for result bindings. "
          + "Note that instances of #$IndeterminateTerm will always be used for explicitly scoped variables "
          + "(e.g. scoped by #$thereExists, etc.).",
          CycObjectFactory.nil, CycObjectFactory.t, null);
  private static final InferenceParameter MAX_NUMBER_PARAM =
          new DefaultIntegerInferenceParameter(null, InferenceParameters.MAX_NUMBER,
          null, "Maximum number of results", "This controls how many answers we want to find before suspending.",
          CycObjectFactory.t, CycObjectFactory.nil, null, 0, Long.MAX_VALUE);
  private static final InferenceParameter MAX_TIME_PARAM =
          new DefaultIntegerInferenceParameter(null, InferenceParameters.MAX_TIME,
          null, "Maximum run time (sec)", "This controls how long we allow an inference to run before suspending.",
          CycObjectFactory.t, CycObjectFactory.nil, null, 0, Long.MAX_VALUE);
  private static final InferenceParameter MAX_TRANSFORMATION_DEPTH_PARAM =
          new DefaultIntegerInferenceParameter(0, InferenceParameters.MAX_TRANSFORMATION_DEPTH,
          null, "Maximum number of transformation steps", "This specifies the maximum allowable number of transformation steps in answers.",
          CycObjectFactory.t, CycObjectFactory.nil, null, 0, Long.MAX_VALUE);
  /// Inference Mode Descriptions
  private static final InferenceParameterValueDescription MINIMAL = new DefaultInferenceParameterValueDescription(
          CycObjectFactory.makeCycSymbol(":MINIMAL"), "Minimal",
          "Sets up minimal defaults for several other inference parameters, including no transformation and no new terms.");
  private static final InferenceParameterValueDescription SHALLOW = new DefaultInferenceParameterValueDescription(
          CycObjectFactory.makeCycSymbol(":SHALLOW"), "Shallow",
          "Sets up shallow defaults for several other inference parameters, including a max transformation depth of 1 and no new terms.");
  private static final InferenceParameterValueDescription EXTENDED = new DefaultInferenceParameterValueDescription(
          CycObjectFactory.makeCycSymbol(":EXTENDED"), "Extended",
          "Sets up extended defaults for several other inference parameters, including a max transformation depth of 2 and new terms allowed.");
  private static final InferenceParameterValueDescription MAXIMAL = new DefaultInferenceParameterValueDescription(
          CycObjectFactory.makeCycSymbol(":MAXIMAL"), "Maximal",
          "Sets up maximal defaults for several other inference parameters, "
          + "including unbounded transformation depth and no restrictions on various other resource constraints.");
  private static final List<InferenceParameterValueDescription> INFERENCE_MODES =
          Arrays.asList(MINIMAL, SHALLOW, EXTENDED, MAXIMAL);
  private static final InferenceParameter INFERERNCE_MODE_PARAM =
          new DefaultEnumerationInferenceParameter(SHALLOW, CycObjectFactory.makeCycSymbol(":INFERENCE-MODE"),
          null, "Inference engine mode",
          "This controls what mode the inference engine will operate under; roughly, "
          + "how deep it will search for an answer before giving up.  "
          + "This is a meta-property that controls many other low-level inference settings.",
          CycObjectFactory.t, CycObjectFactory.nil, null, INFERENCE_MODES);

  public enum OpenCycInferenceMode {

    MINIMAL_MODE(MINIMAL),
    SHALLOW_MODE(SHALLOW), EXTENDED_MODE(EXTENDED), MAXIMAL_MODE(MAXIMAL);
    private final InferenceParameterValueDescription description;

    OpenCycInferenceMode(InferenceParameterValueDescription description) {
      this.description = description;
    }

    public InferenceParameterValueDescription getDescription() {
      return description;
    }

    public static OpenCycInferenceMode fromString(String modeName) {
      for (final OpenCycInferenceMode mode : values()) {
        if (mode.getDescription().getValue().toString().equalsIgnoreCase(modeName)) {
          return mode;
        }
      }
      return null;
    }
  }
}
